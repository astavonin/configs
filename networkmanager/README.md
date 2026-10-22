# NetworkManager dispatcher hook — FortiClient containment

`90-forti-dns-fix` is a NetworkManager dispatcher hook that contains two
FortiClient SSL-VPN misbehaviours on Linux. It handles DNS, and it *detects*
(but does not repair) a routing fault.

The corporate domains it scopes are in `CORP_DOMAINS` at the top of the script.
Change them for another employer; nothing else is site-specific.

## Install

The dispatcher runs scripts as root out of `/etc`, so the repo copy is the
source and the installed copy is a duplicate:

```bash
sudo cp networkmanager/90-forti-dns-fix \
        /etc/NetworkManager/dispatcher.d/no-wait.d/90-forti-dns-fix
sudo chown root:root /etc/NetworkManager/dispatcher.d/no-wait.d/90-forti-dns-fix
sudo chmod 755       /etc/NetworkManager/dispatcher.d/no-wait.d/90-forti-dns-fix

# NetworkManager only enumerates dispatcher.d/ — a bare file in no-wait.d/ never runs.
sudo ln -sfn no-wait.d/90-forti-dns-fix /etc/NetworkManager/dispatcher.d/90-forti-dns-fix
```

`no-wait.d/` means "run detached". The hook sleeps through FortiClient's settle
window, so it must not block the dispatcher queue.

A copy also lives at `~/.config/forti-dns-fix/90-forti-dns-fix` and is what
`sync.sh` tracks — `/etc` needs root, which `sync.sh` does not assume.

Watch it work: `journalctl -t forti-dns-fix -f`

## What it fixes

### 1. DNS hijack (repaired automatically)

FortiClient pushes corporate DNS onto the **physical** link with
`default-route=yes`, discarding the local router's DNS. Every lookup then goes
to servers only reachable inside the tunnel, so when the tunnel drops, all name
resolution dies — not just corporate names.

It does this two ways, and both are handled:

- **Runtime** — while the tunnel is up. The hook moves the pushed DNS onto the
  tunnel link and scopes it to the corporate domains, restoring the router's
  DNS as the default. Applied on `up`, then re-asserted once, because
  FortiClient issues a late reapply that would otherwise overwrite the fix.
- **Persisted** — FortiClient writes `ipv4.dns`, `ipv4.ignore-auto-dns` and
  `ipv4.routes` into the saved NetworkManager profile. Killed rather than
  cleanly disconnected (a reboot stops the service), it never reverts, and the
  next boot applies corporate DNS to wifi with no tunnel to reach it through —
  total DNS failure with no VPN in sight. `sanitize()` strips that residue.

NordVPN (`nordlynx`) and ExpressVPN (`wgexpressvpn`) manage their own DNS and
are deliberately left alone.

### 2. Stale routes after reconnect (detected only, reported)

**The failure.** FortiClient can reconnect onto a **new** `fctvpn*` device while
leaving its `proto static` routes pointing at the previous one, which is by then
`DOWN`. The kernel marks those routes `linkdown` and silently blackholes them.

**Why it is hard to recognise.** The DNS half above keeps working throughout, so
corporate names resolve correctly and only the *connections* hang. Every symptom
points at the remote server rather than at local routing. Observed 2026-10-22:
three `fctvpn` devices, two `DOWN`, with `10.0.0.0/8` and every corporate route
on a dead one — two minutes after the hook had logged a successful split-DNS
apply. Diagnosis cost roughly two hours, spent concluding a healthy internal
service was down.

**What the hook does.** `check_stale_routes()` runs after the DNS work on every
`fctvpn` event, compares the device named by each route against the one that is
actually `UP`, and logs a warning naming the dead device, how many routes point
at it, and the repair command:

```
WARNING: 3 route(s) still point at fctvpn0f59ce80, which is not the live tunnel
(fctvpn4b19ef77). Corporate names will resolve while every connection to them
hangs. Repair: sudo ip route flush dev fctvpn0f59ce80
```

**Why it does not repair automatically.** A wrong `ip route` deletion blackholes
the entire machine; a wrong DNS entry costs one domain. The asymmetry does not
justify automatic mutation for a fault whose real cost was the *time to
identify* it, not the seconds to fix it. Auto-flush is a small change from here
once the detector has demonstrated it fires only when it should.

It runs **after** the DNS work, not before: that path already sleeps through
FortiClient's settle window, so by then FortiClient has finished moving routes
and a disagreement between the route table and the live device is real rather
than transient.

## Design notes

- **Always exits 0.** "Nothing to do" is the common case, and a non-zero exit
  makes NetworkManager log the script as failed.
- **Serialised on a lock.** `up` and its burst of `reapply` events arrive within
  seconds and run in parallel under `no-wait.d/`, so they would otherwise race
  on `resolvectl`.
- **A tunnel counts as active only when its link is `UP`.** A leftover `fctvpn`
  device from a killed session sits there `DOWN`/`NO-CARRIER` and must not be
  mistaken for a live tunnel — the same distinction the route check relies on.

## Verifying

```bash
# healthy: one UP device, routes on it, corporate domains scoped to the tunnel
ip -brief addr show | grep fctvpn
ip route show | grep -c linkdown        # docker0 may legitimately appear
resolvectl domain | grep -A1 fctvpn
```

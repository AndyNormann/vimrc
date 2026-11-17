/**
 * Ade notify: report pi's working/waiting transitions to the Ade host
 * (banner, sound, dock badge, Cmd+J, the tab spinner) over its unix socket.
 *
 * Not an OSC 777 escape: that reaches the host only while a surface is showing the
 * pane, and Ade puts the surfaces of workspaces you are not looking at away.
 *
 * Written by Ade (SettingsManager.ensurePiExtension); edits are overwritten.
 */
import { createConnection } from 'node:net'

export default function (pi: any) {
  const sock = process.env.ADE_SOCKET_PATH ?? `${process.env.HOME}/Library/Application Support/ade/ade.sock`

  function signal(status: string): void {
    // Every session Ade creates carries its tab id in the environment; there is nothing to ask.
    const tab = process.env.ADE_TAB_ID
    if (!tab) return
    // The same word into the session as an OSC 777, which is how it reaches ptyd: the daemon parses
    // every session's own bytes and the app reconciles its tabs against the daemon. A word delivered
    // only over the socket left the daemon believing the tab was idle (or working, from repaints) and
    // the two views drifting apart.
    try {
      process.stderr.write(`\u001b]777;notify;Ade;${status}\u0007`)
    } catch {
      /* no tty: the socket is still the app's business */
    }
    try {
      const c = createConnection(sock)
      c.on('connect', () => {
        c.write(JSON.stringify({ action: 'notify', tab, status }) + '\n')
        c.end()
      })
      c.on('error', () => {})
      c.setTimeout(1000, () => c.destroy())
    } catch {
      /* never break an agent turn over a status ping */
    }
  }

  pi.on('agent_start', () => signal('working'))
  pi.on('agent_settled', () => signal('waiting'))
}
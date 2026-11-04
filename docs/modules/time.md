# time — dates and moments, with no java.time

`okay-effects-time-value`, `okay.effects.time` — depending on nothing, on every platform: Scala.js (a browser, a
Cloudflare Worker) and Native have no java.time of their own, and this is what a program there needs of it.

| | |
|---|---|
| a date | `Date(year, month, day)`: `toString` `2026-11-04`, `Date.parse`, `Date.of` (checked), `epochDay`, `Date.ofEpochDay`, `plusDays`, `dayOfWeek` (1 Monday … 7 Sunday), ordered |
| a moment | `Instant(seconds, nanos)`: `toString` ISO-8601 in UTC (`2026-11-04T14:01:05Z`, `.120` when there is a fraction — java.time's text), `millis`, `plusSeconds`, `date`, `clock` (hour, minute, second), ordered; `Instant.ofMillis`, `Instant.of(date, h, m, s)` |
| read | `Instant.parse(text)`: ISO-8601 with any offset (`Z`, `+02:00`, `+0200`, `-05`; none is UTC), a space for the `T`, seconds optional, a fraction of any length |
| found | `Instant.find(text)`: the first moment in a text, wherever it stands — a log line's in JSON, logfmt, plain or access-log form |

The calendar is proleptic Gregorian, days from the epoch by Howard Hinnant's algorithms. On the JVM the suite holds
both directions to java.time over twenty thousand random moments: the same text, the same seconds, the same date and
day of the week.

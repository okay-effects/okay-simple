# Releasing

Releases are a Maven repository on GitHub Pages: the `gh-pages` branch, its `maven/` directory, served at
https://sergey-scherbina.github.io/okay-simple/maven. No account and no token is needed to use it, only a resolver.

## Using a release

```scala-sbt
resolvers += "okay-effects" at "https://sergey-scherbina.github.io/okay-simple/maven"

libraryDependencies ++= Seq(
  "io.github.sergey-scherbina" %% "okay-effects"         % "0.1.0",
  "io.github.sergey-scherbina" %% "okay-effects-machine" % "0.1.0"
)
```

`%%%` for Scala.js and Scala Native. The libraries are built on Scala 3.3 LTS and Scala 2.13; kyo's interop on the
latest Scala 3, as kyo is.

## Making one

1. On `master`, everything green, on every platform:

   ```sh
   sbt testAll
   sbt -Dplatforms=jvm,js testJS
   sbt -Dplatforms=jvm,native testNative
   ```

2. The version: `ThisBuild / version` in `build.sbt` without `-SNAPSHOT`, committed, tagged (`v0.1.0`).

3. The repository written into `target/pages-maven/`, for both compilers and all three platforms:

   ```sh
   rm -rf target/pages-maven
   sbt -Dplatforms=jvm,js,native publishPages
   ```

4. Onto `gh-pages`, under `maven/`, beside the releases already there:

   ```sh
   git worktree add /tmp/okay-pages gh-pages          # the first time: git worktree add --orphan -b gh-pages /tmp/okay-pages
   mkdir -p /tmp/okay-pages/maven
   rsync -a target/pages-maven/ /tmp/okay-pages/maven/
   touch /tmp/okay-pages/.nojekyll
   git -C /tmp/okay-pages add -A
   git -C /tmp/okay-pages commit -m "release 0.1.0"
   git -C /tmp/okay-pages push origin gh-pages
   git push origin v0.1.0
   ```

   The first time, GitHub Pages is turned on for the branch `gh-pages` in the repository's settings.

5. The version back to the next `-SNAPSHOT`.

A release is never rewritten: a version published is what every build that resolved it has.

{
  lib,
  buildNpmPackage,
  fetchurl,
}:

buildNpmPackage rec {
  pname = "orgnote-cli";
  version = "0.64.0-dev.8f254d4";

  # The published npm tarball ships a pre-bundled dist/index.js (built
  # with @vercel/ncc) but no matching git tag exists upstream for this
  # version, so we fetch straight from the registry rather than GitHub.
  src = fetchurl {
    url = "https://registry.npmjs.org/orgnote-cli/-/orgnote-cli-${version}.tgz";
    hash = "sha256-7pSV8tjJKROULeHBb5wB4jl8PQ9Cd6sCNtbQxxqC7Ww=";
  };

  # npm doesn't publish package-lock.json in the registry tarball, so
  # we vendor one (generated via `npm install --package-lock-only
  # --ignore-scripts --legacy-peer-deps` against this version's
  # package.json) for buildNpmPackage's reproducible-dependency-fetch
  # machinery. --legacy-peer-deps is needed because orgnote-api's
  # vue-router peer range conflicts with the pinned vite devDependency.
  postPatch = ''
    cp ${./orgnote-cli-package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-lBuIAKqMNG3er0SQJBXYBaRMpxbPfZB+bTPRM+/Dur4=";

  # Same vue-router/vite peer conflict as above, but this time it's
  # npmConfigHook's own `npm install` (against the offline cache) that
  # needs to tolerate it.
  npmFlags = [ "--legacy-peer-deps" ];

  # dist/ is already built and is all the "files" field publishes;
  # sqlite3's native binding still gets compiled from its vendored
  # amalgamation source via npmConfigHook's `npm rebuild` step.
  dontNpmBuild = true;

  meta = {
    description = "CLI for synchronizing Org-roam notes with Org Note";
    homepage = "https://github.com/Artawower/orgnote-cli";
    license = lib.licenses.mit;
    mainProgram = "orgnote-cli";
  };
}

# How to add a package to the release workflows

This guide covers setting up a package so it can be released to Hex from this
repository. For why each part is needed, see
[Understanding the Release Process](../explanation/releases.md).

## Add the package to the workflows

Add:

- an entry in `.github/hex-packages.json`, with the GitHub usernames who can
  publish the package in `authorizedUsers`
- an option in the `package` menu of
  `.github/workflows/publish-mix-hex-release.yml`
- a job in `.github/workflows/release-drafter.yml`, with a template in
  `.github/release-drafter-templates/`

## Give the `opentelemetry` Hex user ownership

Skip this for a package that has never been on Hex.

1. Check the package's owners with `mix hex.owner list <package>`.
2. If `opentelemetry` is not an owner, have one of the current owners run
   either:
   - `mix hex.owner add <package> opentelemetry` to keep the current owners
   - `mix hex.owner transfer <package> opentelemetry` to make `opentelemetry`
     the only owner

## Set the starting version

Skip this for a package that has never been on Hex.

The package's first draft is `0.1.0`, regardless of its version on Hex. To make
the drafts continue from the Hex version, create a GitHub release manually for
the latest version on Hex, tagged on the commit it was built from. Use the tag
format from the package's Release Drafter template, such as
`opentelemetry-oban-v1.2.0`.

## Next steps

[Release the package](release-a-package.md).

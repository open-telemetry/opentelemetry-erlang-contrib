# How to release a package to Hex

This guide covers publishing a new version of one package. For how the
workflows fit together, see
[Understanding the Release Process](../explanation/releases.md).

## Before you start

You need:

- the Approver or Maintainer role on the repository
- your GitHub username in the package's `authorizedUsers` list in
  `.github/hex-packages.json`

If the package has never been released from this repository, see
[How to add a package to the release workflows](add-a-package-to-the-release-workflows.md)
first.

## Release the package

1. Check the package's draft on the
   [Releases page](https://github.com/open-telemetry/opentelemetry-erlang-contrib/releases).
   Its title shows the version it will release. To release a different
   version, see [Change the version](#change-the-version).
2. Run
   [Publish Hex Release](https://github.com/open-telemetry/opentelemetry-erlang-contrib/actions/workflows/publish-mix-hex-release.yml)
   with:
   - **Package** set to the name of the package you want to release
   - **Publish release** set to `prep`

   The workflow does a dry run of the Hex publish. If the draft's version is
   higher than the version in the code, it opens a pull request that updates
   the version.
3. In that pull request, rename the `Unreleased` section of the package's
   `CHANGELOG.md` to the new version. Edit it using GitHub's web editor for
   now. We will automate it soon with `git-cliff`. Then merge the pull request.
4. If you want to edit the draft's notes, do it now, since Release Drafter
   rewrites them on every push to `main`.
5. Run
   [Publish Hex Release](https://github.com/open-telemetry/opentelemetry-erlang-contrib/actions/workflows/publish-mix-hex-release.yml)
   again, with **Publish release** set to `publish`.

The release should now be tagged in GitHub, public on the Releases page, and
published on Hex.

## Change the version

1. Change the `major`, `minor` or `patch` labels on the pull requests merged
   since the package's last release. The highest label decides the version. If
   no pull request has a label, the release is considered `minor` by default.
2. Run Release Drafter from the Actions tab to update the draft.

## Troubleshooting

### `publish` opens a pull request and doesn't publish anything

The draft's version is higher than the version in the code. Continue from step
3 of [Release the package](#release-the-package), using the pull request it
opened.

### The workflow fails with `Proposed package version does not increment the current version`

The draft's version is lower than the version in the code.
[Change the version](#change-the-version) so the draft's version is higher.

For a package's first release from this repository, see
[Set the starting version](add-a-package-to-the-release-workflows.md#set-the-starting-version).

### The push to Hex fails

The release and its tag exist, but the package is not on Hex.

1. Fix the cause of the error.
2. Have a Maintainer delete the release and its tag, since tags are protected.
3. Run Release Drafter from the Actions tab to recreate the draft.
4. Run `publish` again.

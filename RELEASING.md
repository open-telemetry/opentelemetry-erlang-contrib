# Releasing Contrib Packages to Hex

Each package is released separately, using two GitHub Actions workflows:

- **Release Drafter** runs on every push to `main`. It keeps a [Draft Release](https://github.com/open-telemetry/opentelemetry-erlang-contrib/releases) for each package, with notes from the merged pull requests that changed that package.
- **Publish Hex Release** publishes a package. An Approver or Maintainer runs it from the Actions tab. See [Who can publish](#who-can-publish) for who can publish each package.

## Who can publish

To publish a package, in addition to having the Approver or Maintainer role on the repo, your GitHub username must be in the package's `authorizedUsers` list in `.github/hex-packages.json`.

The workflow publishes to Hex with the repository secret `OTEL_HEX_KEY`, which belongs to the `opentelemetry` Hex user. It can only publish packages that user owns. Before you set up a package to publish from here, check its owners with `mix hex.owner list <package>`.

## Adding a package

A new package needs:

- an entry in `.github/hex-packages.json`
- an option in the `package` menu of `.github/workflows/publish-mix-hex-release.yml`
- a job in `.github/workflows/release-drafter.yml`, with a template in `.github/release-drafter-templates/`

If the package was already on Hex before it moved to this repository, one of its current owners must add the `opentelemetry` Hex user as an owner before the first release from here. The [new-project pull request template](.github/PULL_REQUEST_TEMPLATE/new_project_template.md) asks for this. Either:

- `mix hex.owner add <package> opentelemetry` to keep the current owners.
- `mix hex.owner transfer <package> opentelemetry` to make `opentelemetry` the only owner.

## The version

Each package's next release starts as a draft on the repository's [Releases page](https://github.com/open-telemetry/opentelemetry-erlang-contrib/releases), visible to Approvers and Maintainers. Release Drafter creates the draft and keeps it up to date on each push to `main`. Its title shows the next version, such as "Opentelemetry Oban - v1.3.0". Publishing the draft creates its configured tag, such as `opentelemetry-oban-v1.3.0`.

Release Drafter picks the version by bumping the package's last release. It uses the highest `major`, `minor` or `patch` label on the pull requests merged since then, or `minor` if none has a label. To change the version, change the labels, then run Release Drafter from the Actions tab to update the draft.

Release Drafter counts only from releases published on the Releases page, not from the version in the code. A package with no release in this repository yet starts from `0.0.0`, so its first draft is `0.1.0`, or `1.0.0` if there is an unreleased PR with a `major` label. If Hex already has a higher version, first create a GitHub release for that version manually, tagged on the commit it was built from.

## Steps

1. Check the package's draft on the [Releases page](https://github.com/open-telemetry/opentelemetry-erlang-contrib/releases).
2. Run [Publish Hex Release](https://github.com/open-telemetry/opentelemetry-erlang-contrib/actions/workflows/publish-mix-hex-release.yml) with:
   - **Package** set to the package
   - **Publish release** set to `prep`

   It does a dry run of the Hex publish. If the draft's version is higher than the version in the code, it opens a pull request that updates it.
3. In that pull request, rename the `Unreleased` section of the package's `CHANGELOG.md` to the new version. Edit it using GitHub's web editor for now. We will automate it soon with `git-cliff`. Then merge the pull request.
4. If you want to edit the draft's notes, do it now, since Release Drafter rewrites them on every push to `main`.
5. Run [Publish Hex Release](https://github.com/open-telemetry/opentelemetry-erlang-contrib/actions/workflows/publish-mix-hex-release.yml) again, with **Publish release** set to `publish`. It:
   - marks the release as no longer a draft, so anyone can see it
   - creates the tag, pointing to the latest commit on `main`
   - publishes the package to Hex

## Troubleshooting

If `publish` opens a version pull request and publishes nothing, the draft's version is higher than the version in the code. Run `prep` first and merge its pull request.

If the workflow fails with `Proposed package version does not increment the current version`, the draft's version is lower than the version in the code. Change the labels so the draft's version is higher. For a package's first release from this repository, see [The version](#the-version).

If the push to Hex fails, the release and its tag will exist, but the package will not be on Hex. Fix the cause of the error, then have a Maintainer delete the release and its tag, since tags are protected. Run Release Drafter from the Actions tab to recreate the draft, then run `publish` again.

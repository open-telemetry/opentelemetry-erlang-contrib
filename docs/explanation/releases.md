# Understanding the Release Process

How releases to Hex work, and the reasoning behind the steps in
[How to release a package to Hex](../how-to/release-a-package.md) and
[How to add a package to the release workflows](../how-to/add-a-package-to-the-release-workflows.md).

## GitHub Actions Workflows

Each package is released separately, using two GitHub Actions workflows.

**Release Drafter** runs on every push to `main`. It maintains a
[Draft Release](https://github.com/open-telemetry/opentelemetry-erlang-contrib/releases)
for each package, with notes from the merged pull requests that changed that
package. Drafts are only visible to Approvers and Maintainers.

**Publish Hex Release** publishes one package. An Approver or Maintainer runs
it from the Actions tab.

Each package has:

- a job in `.github/workflows/release-drafter.yml`
- a template in `.github/release-drafter-templates/`
- an entry in `.github/hex-packages.json`

A new package has to be added in all three places, and to the `package` menu
of `.github/workflows/publish-mix-hex-release.yml`, before it can be released.

## The Draft Decides the Version

The next version of a package comes from its draft, not from the code. The
draft's title shows it, such as "Opentelemetry Oban - v1.3.0", and publishing
the draft creates its configured tag, such as `opentelemetry-oban-v1.3.0`.

Release Drafter picks the version by bumping the package's last release. It
uses the highest `major`, `minor` or `patch` label on the pull requests merged
since then, or `minor` if none has a label. The labels are therefore the way to
change the version.

Release Drafter counts only from releases published on the Releases page, not
from the version in the code. A package with no release in this repository yet
starts from `0.0.0`, so its first draft is `0.1.0`, or `1.0.0` if there is an
unreleased pull request with a `major` label. A package that was on Hex before
it moved here can already have a higher version than that, which is why its
first release needs a GitHub release created manually for the version on Hex.

## Why Publishing Takes Two Runs

The version in the code has to match the draft before anything is published.
Publish Hex Release compares the two on every run:

- If they match, the release is ready.
- If the draft's version is higher, it opens a pull request that updates the
  version in the code, and does not publish.
- If the draft's version is lower, it fails with
  `Proposed package version does not increment the current version`.

The `prep` action gets `main` ready for the release and never publishes. It
does a dry run of the Hex publish with the new version, then opens the version
pull request. Until that version is on `main`, `publish` does the same as
`prep`.

The `publish` action proceeds only when the versions match. It:

- marks the release as no longer a draft, so anyone can see it
- creates the tag, pointing to the latest commit on `main`
- publishes the package to Hex

The GitHub release is published before the package is pushed to Hex. If the
push to Hex fails, the release and its tag exist but the package is not on Hex.
Tags are protected, so only a Maintainer can delete them to let the release run
again.

## Why the Draft's Notes Are Edited Last

Release Drafter rewrites a draft's notes on every push to `main`, and merging
the version pull request is a push to `main`. Notes edited before that merge
are lost, so the notes are edited after it, just before publishing.

## Who Can Publish

The workflow publishes to Hex with the repository secret `OTEL_HEX_KEY`, which
belongs to the `opentelemetry` Hex user, so maintainers should not publish with
their own Hex account. Instead, the workflow checks that the GitHub user who
ran it is in the package's `authorizedUsers` list in
`.github/hex-packages.json`.

The key can only publish packages that the `opentelemetry` Hex user owns. A
package that was already on Hex before it moved to this repository has other
owners, so one of them must add `opentelemetry` as an owner before the first
release from here. The
[new-project pull request template](../../.github/PULL_REQUEST_TEMPLATE/new_project_template.md)
asks for this.

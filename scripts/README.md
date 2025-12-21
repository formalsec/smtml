# Creating a release

SOP to create a new release in smtml.

## Semi-automatically

1. Trigger the [Release PR workflow].
2. Approve the PR and merge changes to **main**.
3. Locally, pull the changes and tag the latest commit with `v[VERSION]`, where `VERSION` is the version created by the workflow.
4. Push the tag.
4. Re-run the latest [opam-repository CI] to automatically publish the package.
5. Track the created PR in the opam-repository to check for CI errors or comments from maintainers.

**Note**: There is a cron job in GitHub that is set to run the [Release PR workflow] every monday at 9AM.

[Release PR workflow]: https://github.com/formalsec/smtml/actions/workflows/release.yml
[opam-repository CI]: https://github.com/formalsec/smtml/actions/workflows/publish.yml

## Manually

```sh
# 1. Run the create-release.sh script:
#   a. If no version is specified, the create-release.sh script simply bumps the current version.
./create-release.sh
#   b. If specified, it will use the provided tag instead. Example:
# ./create-release.sh 0.18.0

# 2. Approve the created PR and merge it to main.

# 3. Update the main branch:
git switch main
git fetch --all
git rebase

# 4. Create the tag:
git tag -a vVERSION # Where VERSION is the desired version
git push -u origin vVERSION

# 5. Publish new version to the opam-repository:
./publish.sh
```

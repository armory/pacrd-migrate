# pacrd-migrate

This repository provides a small tool for migrating from [PaCRD manifests] to
[Dinghy] pipeline definitions. It will take `Application` and `Pipeline`
definitions and convert them into corresponding Dinghyfiles.

## Installation

### from GitHub

`pacrd-migrate` can be installed from GitHub Releases. To install the latest
version run the following commands.

For Linux users:

```bash
bash -c \
  'curl -fsSL -o pacrd-migrate "https://github.com/armory/pacrd-migrate/releases/latest/download/pacrd-migrate-linux-amd64"' \
  && chmod +x pacrd-migrate
```

For macOS users:

```bash
bash -c \
  'curl -fsSL -o gah "https://github.com/armory/pacrd-migrate/releases/latest/download/pacrd-migrate-macos-amd64"' \
  && chmod +x pacrd-migrate
```

If you want to install a specific release, you can naviate to the [releases page]
and grab the URL for the desired release to download.

You can then move the binary somewhere in your environment's `$PATH`.

_Note: at this time only Linux binaries are statically compiled._

### from Source

`pacrd-migrate` can optionally be compiled from source. You will need the
[`stack`] program to build this project.

You can build and copy the binary for this project by running the following
command:

```bash
stack build --copy-bins
```

This may take a while if you don't usually work with Haskell projects, feel
free to grab a beverage of your choice and come back in 5-10 minutes.

## Usage

`pacrd-migrate` operates on directories of PaCRD manifests. It reads in each
manifest and then converts them into the fewest possible Dinghyfiles. Dinghy
module are not supported, since no equivalent primitive exists in PaCRD.

To convert a directory of manifests:

```shell
pacrd-migrate --input-path path/to/pacrd/manifests/ --output-path /path/to/dinghy/
```

You can optionally specify a single file to convert:

```shell
pacrd-migrate --input-path path/to/pacrd/manifest/pipeline.yaml --output-path /path/to/dinghy/
```

Short options also exist for flags

```shell
pacrd-migrate -i /input/path -o /output/path
```

[PaCRD manifests]: https://docs.armory.io/docs/spinnaker-user-guides/pacrd/
[Dinghy]: https://docs.armory.io/docs/armory-admin/dinghy-enable/
[releases page]: https://github.com/armory/pacrd-migrate/releases
[`stack`]: https://docs.haskellstack.org/en/stable/README/

# Yaml Shyaml - Tools for manipulating YAML

The `json` command just flips the format from YAML to json.

The `diff` command will diff the contents of the yaml, not the actual
text. This is needed since sometimes keys are printed in different
order. -- Note this command is pretty hard to make useful.

The `sortkeys` command will print the yaml out again (not necessarily
formatted the same way) but with the keys of all mappings sorted.

The `kubediff` command will read a kubernetes resource yaml and
compare it to the one on the server, printing out a diff. It relies
on kubectl being installed and configured on the system.

## Build & Install

You'll need [rust](https://www.rust-lang.org/en-US/install.html) to build
`shyaml`.

Once you've cloned this repo, just use cargo to run shyaml:

```
cargo sortkeys ~/a_yaml_file.yaml
cargo run kubediff ~/kubefiles/foo.yaml
```

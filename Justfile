# Parameters
set unstable := true
podman := which("podman") || require("podman-remote")
mode := env("MODE", "development")
hugo_image := "cgr.dev/chainguard/hugo"
hugo_dir := "/hugo"

hugo := podman + ' run -it --rm \
    -u 0 \
    --security-opt label=disable \
    --name chronoscrat.io \
    -v $(pwd):' + hugo_dir + ':Z \
    -p 8080:8080 \' + hugo_image

# Config
[group("config")]
fetch-submodules:
    git submodule update --init --recursive 

# Serve
[group("serve")]
serve:
    #!/usr/bin/bash

    {{ hugo }} \
        serve -D --bind 0.0.0.0 --port 8080

[group("content")]
new kind="" path="" title="":
    #!/usr/bin/bash

    {{ hugo }} new content --kind {{ kind }} {{ path }}/{{ title }}

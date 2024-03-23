See below for making python and jupyter extensions available in the Dev Container too:
* [Why aren't some of my extensions enabled when I open a VS Code remote workspace?](https://stackoverflow.com/a/55958396)
* ["Always installed" Features](https://code.visualstudio.com/docs/devcontainers/containers#_always-installed-features)
* [User and Workspace Settings](https://code.visualstudio.com/docs/getstarted/settings)

To edit `remote.containers.defaultExtensions`, go to Dev Container Settings from the F1 menu. Here is an example list of useful extensions:

```json
"dev.containers.defaultExtensionsIfInstalledLocally": [
        "GitHub.copilot",
        "GitHub.copilot-chat",
        "GitHub.vscode-pull-request-github",
        "ms-python.python",
        "ms-toolsai.jupyter",
        "mutantdino.resourcemonitor"
    ]
```


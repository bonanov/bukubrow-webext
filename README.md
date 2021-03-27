TODO: https://github.com/purescript/spago#monorepo

Bukubrow
===

Bukubrow is a WebExtension for [Buku](https://github.com/jarun/Buku), a command-line bookmark manager.

- Display, open, add, edit, and delete bookmarks
- Automatically save open tabs to the _staging area_ from the context menu, from which they can be optionally edited and saved
- Filter bookmarks with any of the following syntax: `:url`, `>description`, `#tag`, `*wildcard`
- Bookmarklet (arbitrary JavaScript scripting) support, simply prepend your "URL" with `javascript:`, for example: `javascript:document.body.style.background = 'red'`
- Custom hotkeys are available - please read the instructions [here](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/commands#Updating_shortcuts) to customise them in your browser

## Prerequisites

A corresponding [native host](https://github.com/SamHH/bukubrow-host) is used to interface with your Buku database. Communication between the host and the browser extension is handled via [native messaging](https://developer.chrome.com/extensions/nativeMessaging).

- Buku
- Bukubrow Host
- Supported browser: Firefox, Chrome, or Chromium
- _If building the WebExtension_:
	- Node
	- Yarn (or npm)
    - [Spago](https://github.com/purescript/spago) (PureScript)

## Installation

Installing the host and registering it with your browser is required to allow the browser extension to talk to Buku.

Install the WebExtension from the relevant addon store.

- Chrome/Chromium: https://chrome.google.com/webstore/detail/bukubrow/ghniladkapjacfajiooekgkfopkjblpn
- Firefox: https://addons.mozilla.org/en-US/firefox/addon/bukubrow/

Alternatively, you can build the WebExtension manually as follows:

1. Clone the repo.
2. Run `make release`. Your zip file will be located within the `./release/` directory. This zip file is the exact structure expected by all compatible browsers.
3. Load the extension in your browser. Please refer to your browser's documentation.

## Contributing

The WebExtension is written in PureScript, utilising [Halogen](https://github.com/purescript-halogen/purescript-halogen) for UI rendering. Data is fetched from the host via native messaging.

As referenced above Spago is the build tool of choice, thus the only scripts defined in the makefile related to building are those for bundling Spago/PureScript's output for the browser. A typical development workflow is to run `make -s bundle-dev`, `spago build -w`, and `spago test -w` simultaneously.


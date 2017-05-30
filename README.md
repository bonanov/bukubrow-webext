Bukubrow
=======

Bukubrow is a WebExtension for [Buku](https://github.com/jarun/Buku), a command-line bookmark manager. This WebExtension is developed to work in at least Chrome, Chromium, and Firefox, however any other browsers that support WebExtensions should also run this just fine.

It uses a [native binary written in Golang](https://github.com/samhh/Bukubrow/blob/master/binary/bukubrow.go) to interface with your Buku database. Secure communication between the binary and the browser extension is handled through [native messaging](https://developer.chrome.com/extensions/nativeMessaging).

This project has been heavily influenced by [browserpass](https://github.com/dannyvankooten/browserpass), a WebExtension designed to allow a similar kind of synchronicity between the browser and [pass](https://www.passwordstore.org).

## Requirements

- Buku
- A web browser that supports recent web standards and WebExtensions; this includes all recent releases of Chrome, Chromium, and Firefox.

## Installation

#### Step 1 - Installing the binary

Start out by downloading the [latest binary](https://github.com/samhh/Bukubrow/releases) for your operating system. A prebuilt binary for 64-bit macOS is available. For Linux and/or 32-bit systems you will need to compile the binary yourself until I figure out how to cross-compile (help appreciated!). See below for instructions.

1. Extract the package.
2. Run `./install.sh` to install the native messaging host. If you want a system-wide installation, run the script with `sudo`.

Installing the binary and registering it with your browser through the installation script is required to allow the browser extension to talk to Buku.

Note that the binary must always remain in the same location in order for the browser to find it. If you move or delete the file you will need to install it / register it with the browser again.

#### Step 2 - Installing the WebExtension

Install the WebExtension from the relevant addon store.

- Chrome: https://chrome.google.com/webstore/detail/bukubrow/ghniladkapjacfajiooekgkfopkjblpn
- Firefox: https://addons.mozilla.org/en-US/firefox/addon/bukubrow/ (Note: that this is presently quite far behind master)

## Building the binary

Clone the repo, run `make binary-linux-x64` (or substitute `binary-linux-x64` for your preferred target) and then inside `./release` you'll have a zip identical to what you would have downloaded as a binary in step 1.

## Building the WebExtension

Clone the repo, run 'make webext` and then inside `./release` you'll have a zip containing all required files and folders in the expected structure.

## Building everything

You can alternatively build everything all at once with `make release`.

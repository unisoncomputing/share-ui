# Unison Share UI

[![CI](https://github.com/unisoncomputing/unison-share-ui/actions/workflows/ci.yml/badge.svg)](https://github.com/unisoncomputing/unison-share-ui/actions/workflows/ci.yml)

This repository contains the implementation of the Unison Share front end. It talks to the [`@unisoncomputing/share-api`](https://github.com/unisoncomputing/share-api) API server backend.

## Running Development Server

Prerequisites: `node v20` or higher.

1. Start [`Share API`](https://github.com/unisoncomputing/share-api)
2. Make sure the latest dependencies are installed with by running `npm
install`.
3. Start the dev server with: `API_URL="<SHARE API URL>" npm start`

Note that Share API hosts on `http://localhost:5424` be default, so its
likely what you'll run with, but you can always see the URL when starting
Share API.

4. Visit `http://localhost:1234` in a browser.

## Dependencies

This depends on the [ui-core package](https://github.com/unisonweb/ui-core) via
[elm-git-install](https://github.com/robinheghan/elm-git-install). That package
includes both the Unison design system, and a core set of components for
working with and rendering Unison definitions and
namespaces.

## Bumping [ui-core package](https://github.com/unisonweb/ui-core)

The UI Core dependency can be updated to its latest version with this command:

```bash
npm run ui-core-update
```

To install a specific sha:

```bash
npm run ui-core-install -- [SOME_UI_CORE_SHA]
```

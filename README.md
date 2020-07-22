This simple UI can be used to experiment with Autoteka API or can be viewed as an example of not really correct usage of https://github.com/ruamc/autoteka package.

## How to build

- **fronetend**

```
$ npm install
$ parcel build index.html
```

- **backend**

```
$ stack build
```

## How to run

You need valid API credentials to run backend.

```
CLIENT_ID="gQD..." CLIENT_SECRET="9vG..." stack run
```

## Warning

The code is quick an dirty.

There are some hardcoded values:
  - backend listens to port `3000`
  - backend serves frontend from `./dist`
  - token refresh interval is 20 minutes

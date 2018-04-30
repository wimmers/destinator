# Destinator

# Build

## Run Project

```sh
npm install
npm start
# in another tab
npm run webpack
```
After you see the webpack compilation succeed (the `npm run webpack` step), open up `src/index.html` (**no server needed!**).

## Build for Production

```sh
npm run build
npm run webpack:production
```

This will replace the development artifact `build/Index.js` for an optimized version.

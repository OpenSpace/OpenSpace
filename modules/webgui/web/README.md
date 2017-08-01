# OpenSpace WebGUI Module

## Develop

First, go get [Node.js](https://nodejs.org/en/)!

```sh
# install dependencies
npm install
# run development app
npm start
# open gui
open http://localhost:8080
```

### Types

This is something you hear quite often:

> MEH! JavaScript is the worst, there's no types or anything. Boo!! <br> - *Some developers*

It is not true. JavaScript has lots of types, but they might be a bit scary for the developers who's into strict typing.

To make this a bit easier for y'all, this web app has support for types using the Flow system. In some files, you might see a comment looking like 

```js
// @flow
```

This tells our transpiler Babel (that makes our modern JavaScript readable for all browsers) that this file should be type checked! 
A type declaration looks like

```js
function stringReturner(a: string, bMightBeAnything): string {
  //                     ^^^^^^^^                   ^^^^^^^^
  // The interesting parts of this code snippet is higlighted above 
}
```

For more about Flow, check out https://flow.org

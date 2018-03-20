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

### Components

There are several useful and reusable components to make it easier for you as the developer. Most of 
these general-purpose components are in [`src/comoponents/common`](src/components/common). To see
some of them, [Storybook](https://github.com/storybooks/storybook) is a useful tool. It allows you
to see them and try them out. When you are developing common-use components, please add stories for 
them. 

#### Using Storybook

To start storybook, run 

```sh
npm run storybook
open http://localhost:9001
``` 

in your favourite terminal. This will start Storybook and open it up in your browser.

To add stories, add a file called `WhateverYourComponentNameIs.story.jsx`. It will get picked up by
Storybook. The `.story.jsx` ending is the important part of the file name, as this is what the 
Storybook config is looking for. See the [Storybook documentation](https://storybook.js.org/basics/introduction/) 
for instructions on how to build stories. 

Using Storybook, you will help 

* your fellow developers understand how a component may be used, and
* yourself in writing a clear, friendly API for your components.

Please try and declare every special case in your stories, as that will further improve the 
usefulness of the tool.

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

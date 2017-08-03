// @flow

export function excludeKeys(value: Object, keys: string): Object {
  const doNotInclude = keys.split(' ');
  return Object.keys(value)
    // actually filter out the keywords
    .filter(key => !doNotInclude.includes(key))
    // create new object to return
    .reduce((newObj, key) => {
      // eslint-disable-next-line no-param-reassign
      newObj[key] = value[key];
      return newObj;
    }, {});
}

/**
 * Run a function `func` after current call stack has been finished
 *
 * @param func - the function to call
 * @param args - the arguments to call with
 */
export const defer = (func: Function, ...args: Array<mixed>): number =>
  setTimeout(() => func(...args), 0);

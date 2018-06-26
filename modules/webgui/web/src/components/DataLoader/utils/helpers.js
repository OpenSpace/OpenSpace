
/**
 * 
 * @param {string} stringList = A string list from the StringListProperty in OpenSpace
 */
export const stringListToArray = (stringList) => {
  return stringList.split(',').map(item => item.trim());
}

/**
 * 
 * @param {string} directory 
 */
export const getDirectoryLeaf = (directory) => {
  return directory.match('([^/]+)/?$')[0];
}

/**
 * Replace the various OS (win, macOS, Linux) types
 * of line break characters with whitespaces
 * @param {string} input - the input text
 */
export const removeLineBreakCharacters = (input) => {
  return input.replace(/\r?\n|\r/g, '');
}


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

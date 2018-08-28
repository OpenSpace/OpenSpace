
/**
 * 
 * @param {string} stringList = A string list from the StringListProperty in OpenSpace
 */
export const stringListToArray = (stringList) => {
  return stringList.split(',').map(item => item.trim());
}

/**
 * 
 * @param {string} filePath 
 */
export const backSlashesToForward = (filePath) => {
  return filePath.replace(/\\/g, '/');
}
/**
 * 
 * @param {string} directory - A file system path
 * @returns The last word/filename/directory separated by slashes
 */
export const getDirectoryLeaf = (directory) => {
  const withFixedSlashes = backSlashesToForward(directory);
  return withFixedSlashes.match('([^/]+)/?$')[0];
}

/**
 * Replace the various OS (win, macOS, Linux) types
 * of line break characters with whitespaces
 * @param {string} input - the input text
 */
export const removeLineBreakCharacters = (input) => {
  return input.replace(/\r?\n|\r/g, '');
}

/**
 * @param {string} filename - the input file name with extension
 */
export const getFileBasename = (filename) => {
  return filename.replace(/\.[^.]+$/, '');
}

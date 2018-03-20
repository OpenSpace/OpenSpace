// @flow

/**
 * Check if search is a substring of test
 * @param test - string to match against
 * @param search - string to match with
 * @constructor
 */
export const SimpleSubstring = (test: string, search: string): bool => test.includes(search);

// Function for converting the focus node list to an array
export const fromStringToArray = string => string.replace(/['"]+/g, '').split(', ');

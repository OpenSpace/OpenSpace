function parseJson(elementId) {
  var jsonElement = document.getElementById(elementId);
  return JSON.parse(jsonElement.innerHTML);
}

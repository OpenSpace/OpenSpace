function copyTextToClipboard(text) {
  var textArea = document.createElement("textarea");
  textArea.style.position = 'fixed';
  textArea.style.top = 0;
  textArea.style.left = 0;

  textArea.style.width = '2em';
  textArea.style.height = '2em';

  textArea.style.padding = 0;

  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';

  textArea.style.background = 'transparent';
  textArea.value = text;

  document.body.appendChild(textArea);

  textArea.select();
  document.execCommand('copy');

  document.body.removeChild(textArea);
}

window.onload = function () {
  var mainTemplateElement = document.getElementById('mainTemplate');
  var mainTemplate = Handlebars.compile(mainTemplateElement.innerHTML);

  var propertyOwnerTemplateElement = document.getElementById('propertyOwnerTemplate');
  Handlebars.registerPartial('propertyOwner', propertyOwnerTemplateElement.innerHTML);

  var propertyTemplateElement = document.getElementById('propertyTemplate');
  Handlebars.registerPartial('property', propertyTemplateElement.innerHTML);

  Handlebars.registerHelper('urlify', function(options, context) {  
    var data = context.data;
    var identifier = options.replace(" ", "-").toLowerCase();

    while (data = data._parent) {
      if (data.key !== undefined) {
        identifier = data.key + "-" + identifier;
      }
    }
    return identifier;
  });

  var data = {
    propertyOwners: propertyOwners,
    version: version,
    sceneFilename: sceneFilename,
    generationTime: generationTime
  }

  var contents = mainTemplate(data);
  document.body.innerHTML = contents;
}

window.onload = function () {
  var mainTemplateElement = document.getElementById('mainTemplate');
  var mainTemplate = Handlebars.compile(mainTemplateElement.innerHTML);

  var sceneLicenseTemplate = document.getElementById('sceneLicenseTemplate');
  Handlebars.registerPartial('scenelicense', sceneLicenseTemplate.innerHTML);

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
    sceneLicenses: sceneLicenses,
    version: version
  }

  var contents = mainTemplate(data);
  document.body.innerHTML = contents;
}
window.onload = function () {
  var mainTemplateElement = document.getElementById('mainTemplate');
  var mainTemplate = Handlebars.compile(mainTemplateElement.innerHTML);

  var scriptingTemplateElement = document.getElementById('scriptingTemplate');
  Handlebars.registerPartial('scripting', scriptingTemplateElement.innerHTML);

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

  scripting.sort(function (a, b) {
    return a.library < b.library ? -1 : (a.library > b.library ? 1 : 0);
  });

  var data = {
    scripting: scripting,
    version: version
  }

  var contents = mainTemplate(data);
  document.body.innerHTML = contents;
}
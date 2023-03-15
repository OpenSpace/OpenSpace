window.onload = function () {
  var mainTemplateElement = document.getElementById('mainTemplate');
  var mainTemplate = Handlebars.compile(mainTemplateElement.innerHTML);

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

  Handlebars.registerHelper('level', function(options, context) {
    var data = context.data;
    var level = 0;

    while (data = data._parent) {
      if (data.key !== undefined) {
        ++level;
      }
    }

    return level;
  });

  var dataElement = document.getElementById('data');
  var data = JSON.parse(dataElement.innerHTML);

  var contents = mainTemplate(data);
  document.body.innerHTML = contents;
}

var parseJson = (elementId) => {
  var jsonElement = document.getElementById(elementId);
  return JSON.parse(jsonElement.innerHTML);
};

var urlifyFunction = (options, context) => {  
	var data = context.data;

  var identifier = options.replace(/ /g, '');
	// while (data) {
 //    data = data._parent
	//   if (data && data.key !== undefined) {
	//     identifier = data.key + "-" + identifier;
	//   }
	// }

	return identifier;
};


var copyTextToClipboard = (text) => {
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


var sidebarNavigate = (index) => {
  var subtreeSelector = "#index" + index + "-subtree";

  if ((currentDocumentation == documentation[index]) 
    && (document.querySelector(subtreeSelector).innerHTML != "")) 
  {
      var subtreeSelector = "#index" + index + "-subtree";
      document.querySelector(subtreeSelector).innerHTML = "";
      return;
  }
  currentDocumentation = documentation[index];
  var template = templates[currentDocumentation.identifier];
  var html = template(currentDocumentation);
  document.getElementById('current-documentation-container').innerHTML = html;
  //empty existing subtreee
  document.querySelectorAll('.sidebar-subtree').forEach((e) => {e.innerHTML = ''});
  //create current subtree
  var subTreeHTML = "";
  for (var i = 0; i < currentDocumentation.data.length; ++i) {
    var entry = currentDocumentation.data[i];
    var entryProp = "name";
    if (!entry[entryProp]) {
      entryProp = "library";
      if (!entry[entryProp]) {
        entryProp = "key";
      }
    }

    var entryValue = entry[entryProp];
    var entryURL = entryValue;
    switch (currentDocumentation.identifier) {
      case 'scripting':
        entryURL = "openspace" + (entryValue ? "." + entryValue : "");
        entryValue = entryURL;
        break;
      default:
        entryURL = urlifyFunction(entryValue, entry);
    }

    subTreeHTML += "<li>";
    subTreeHTML += "<a href='#" + entryURL + "'>" + entryValue + "</a>";
    subTreeHTML += "</li>";
  }

  //find our subtree and fill it with data 
  document.querySelector(subtreeSelector).innerHTML = subTreeHTML;
};

window.onload = function () {
  var mainTemplateElement = document.getElementById('mainTemplate');
  var mainTemplate = Handlebars.compile(mainTemplateElement.innerHTML);

  window.registerTemplates();

  Handlebars.registerHelper('urlify', urlifyFunction);

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

  for (var i = 0; i < documentation.length; i++) {
    documentation[i].data.sort(function (a, b) {
      return a.name < b.name ? -1 : (a.name > b.name ? 1 : 0);
    });

    if (documentation[i].identifier == "propertylist") {
      for (var j = 0; j < documentation[i].data.length; j++) {
        if (documentation[i].data[j].name == "Scene") {
          documentation[i].data.splice(j, 1);
        }
      }
    }

    if (documentation[i].identifier == "sceneLicense") {
      for (var j = 0; j < documentation[i].data.length; j++) {
        var escaped = documentation[i].data[j].description.replace(
          /\\n/g, ""
        );
        documentation[i].data[j].description = escaped;
        var escapedLicense = documentation[i].data[j].license.replace(
          /\\n/g, ""
        );
        documentation[i].data[j].license = escapedLicense;
      }
    }

  }

  currentDocumentation = documentation[3];

  var data = {
    documentation: documentation,
    version: version,
    currentDocumentation: currentDocumentation
  }

  var templates = {};

  var contents = mainTemplate(data);
  document.body.innerHTML = contents;
}

var levels = ['trace', 'debug', 'info', 'warning', 'error', 'fatal'];
var filterLevel = 0;

function insertAfter(newNode, referenceNode) {
  referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
}

function remove(node) {
  node.parentNode.removeChild(node);
}

function scrollToPosition(selector) {
  var element = document.querySelector(selector);
  if (element && element.scrollIntoView) {
    element.scrollIntoView();
  }
}

function getLevel(element) {
  return levels.findIndex(function (levelString, level) { 
    var className = 'log-level-' + levelString;
    if (element.classList.contains(className)) {
      return true;
    }
  });
}

function pluralize(nItems, singular, plural) {
  if (nItems === 1) {
    return [1, singular].join(' ');
  }
  plural = plural || [singular, 's'].join('');
  return [nItems.toString(), plural].join(' ');
}

function scrollLink(content, selector) {
  var html =  '<a onclick="scrollToPosition(\'' + selector + '\')">' + content + "</a>";
  return html;
}

function getSummary() {
  var nFatals = document.getElementsByClassName('log-level-fatal').length;
  var nErrors = document.getElementsByClassName('log-level-error').length;
  var nWarnings = document.getElementsByClassName('log-level-warning').length;

  if (nFatals > 0) {
    return '<span class="summary summary-fatal">' + scrollLink(pluralize(nFatals, 'fatal error'), '.log-level-fatal') +
    ', ' +  scrollLink(pluralize(nErrors, 'other error'), '.log-level-error') + ' and ' + scrollLink(pluralize(nWarnings, 'warning'), '.log-level-warning') + '</span>';
  } else if (nErrors > 0) {
    return '<span class="summary summary-error">' + scrollLink(pluralize(nErrors, 'error'), '.log-level-error') + ' and ' +
    scrollLink(pluralize(nWarnings, 'warning'), '.log-level-warning') + '</span>';
  } else if (nWarnings > 0) {
    return '<span class="summary summary-warning">' + scrollLink(pluralize(nWarnings, 'warning'), '.log-level-warning') + '</span>';
  } else {
    return '<span class="summary summary-ok">No errors or warnings</span>';
  }
}

function updateFilter() {
  var table = document.getElementsByTagName('table')[0];
  table.classList.remove('hidden');

  var noMessages = document.getElementById('no-messages');
  if (noMessages) {
    remove(noMessages);
  }

  var rows = document.getElementsByTagName('tr');
  var nShown = 0;
  [].forEach.call(rows, function (row) {
    if (row.classList.length === 0) {
      return;
    }
    var rowLevel = getLevel(row);
    if (rowLevel >= filterLevel) {
      row.classList.remove('hidden');
      nShown++;
    } else {
      row.classList.add('hidden');
    }
  });
  if (nShown === 0) {
    var select = document.getElementsByTagName('select')[0];
    var p = document.createElement("p");
    p.id = "no-messages";
    p.innerHTML = "There are no log messages with the level '" + levels[filterLevel] + "' or higher.";
    insertAfter(p, select);
    table.classList.add('hidden');
  }
}

window.onload = function () {
  var header = document.getElementsByTagName('h1')[0];
  header.innerHTML = "OpenSpace Log";

  var summary = document.createElement('p');
  summary.innerHTML = getSummary();

  var select = document.createElement('select');
  select.id = 'filter-level-selector';

  var selectLabel = document.createElement('label');
  selectLabel.for = 'filter-level-selector';
  selectLabel.innerHTML = "Lowest log level to show: ";

  levels.forEach(function (level) {
    var option = document.createElement('option');
    option.value = level;
    option.innerHTML = level;
    select.appendChild(option);
  });

  insertAfter(summary, header);
  insertAfter(selectLabel, summary);
  insertAfter(select, selectLabel);

  var preselectedIndex = levels.indexOf(window.location.hash.slice(1));
  if (preselectedIndex >= 0) {
    filterLevel = select.selectedIndex = preselectedIndex;
    updateFilter();
  }

  select.onchange = function (evt) {
    filterLevel = select.selectedIndex;
    updateFilter();
    window.location.hash = '#' + select.options[select.selectedIndex].value;
  };
}
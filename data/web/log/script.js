var sessionData = {};
try {
  sessionData = JSON.parse(decodeURIComponent(window.location.hash.slice(1)));
} catch (e) {}

var levels = ['trace', 'debug', 'info', 'warning', 'error', 'fatal'];
var filterLevel = 0;

function insert(newNode, parentNode) {
  parentNode.insertBefore(newNode, null);
}

function insertBefore(newNode, referenceNode) {
  referenceNode.parentNode.insertBefore(newNode, referenceNode);
}

function insertAfter(newNode, referenceNode) {
  referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
}

function remove(node) {
  node.parentNode.removeChild(node);
}

function scrollToPosition(selector) {
  var element = document.querySelector(selector);
  if (element && element.scrollIntoView) {
    var rect = element.getBoundingClientRect();
    // Header is 80 px, 40 px margin
    window.scrollTo(window.scrollX, + window.scrollY + rect.top - 80 - 40);
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
    var header = document.getElementsByTagName('header')[0];
    var p = document.createElement("p");
    p.id = "no-messages";
    p.innerHTML = "There are no log messages with the level '" + levels[filterLevel] + "' or higher.";
    insertAfter(p, header);
    table.classList.add('hidden');
  }
}

var reloadHandle = 0;

function scheduleReload() {
  updateSessionData({
    reload: true
  });
  reloadHandle = window.setTimeout(function () {
    window.location.reload();
  }, 5000);
}

function unscheduleReload() {
  updateSessionData({
    reload: false
  });
  window.clearTimeout(reloadHandle);
}

function updateSessionData(obj) {
  var changed = false;
  Object.keys(obj).forEach(function (k) {
    if (sessionData != obj[k]) {
      sessionData[k] = obj[k];
      changed = true;
    }
    if (!sessionData[k]) {
      delete sessionData[k];
    }
  });
  if (changed) {
    window.location.hash = '#' + JSON.stringify(sessionData);
  }
}

window.onload = function () {
  var header = document.createElement('header');

  var headerTitle = document.createElement('h1');
  headerTitle.innerHTML = "OpenSpace Log";

  var summary = document.createElement('p');
  summary.id = 'summary';
  summary.innerHTML = getSummary();

  var selectFilter = document.createElement('select');
  selectFilter.id = 'filter-level-selector';

  var selectFilterGroup = document.createElement('div');
  var selectFilterLabel = document.createElement('label');
  selectFilterLabel.innerHTML = "Filter level";

  levels.forEach(function (level) {
    var option = document.createElement('option');
    option.value = level;
    option.innerHTML = level;
    selectFilter.appendChild(option);
  });


  var autoReloadToggle = document.createElement('input');
  autoReloadToggle.type = 'checkbox';

  if (sessionData.reload) {
    scheduleReload();
    autoReloadToggle.checked = true;
    if (sessionData.trackBottom) {
      window.setTimeout(function () {
        window.scrollTo(window.scrollX, document.body.scrollHeight);
      }, 1);
    }
  }

  autoReloadToggle.onchange = function () {
    if (autoReloadToggle.checked) {
      scheduleReload();
    } else {
      unscheduleReload();
    }
  };

  var autoReloadLabel = document.createElement('label');
  autoReloadLabel.innerHTML = "Auto-reload";

  var autoReloadGroup = document.createElement('div');

  insertBefore(header, document.getElementsByTagName('table')[0]);
  insert(headerTitle, header);
  insert(summary, header);

  insert(selectFilterLabel, selectFilterGroup)
  insert(selectFilter, selectFilterGroup);
  insert(selectFilterGroup, header);

  insert(autoReloadLabel, autoReloadGroup);
  insert(autoReloadToggle, autoReloadGroup);
  insert(autoReloadGroup, header)


  var preselectedIndex = levels.indexOf(sessionData.filter || 'trace');
  if (preselectedIndex >= 0) {
    filterLevel = selectFilter.selectedIndex = preselectedIndex;
    updateFilter();
  }

  selectFilter.onchange = function (evt) {
    filterLevel = selectFilter.selectedIndex;
    updateFilter();
    updateSessionData({
      filter: selectFilter.options[selectFilter.selectedIndex].value
    });
  };

  window.onscroll = function () {
    var scroll = document.documentElement.scrollTop || document.body.scrollTop;
    scroll += window.innerHeight;
    updateSessionData({
      trackBottom: scroll > document.body.scrollHeight - 10
    });
  }
}

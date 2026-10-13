// whence.js - the browser side of whenceOutput(), whenceTableOutput() and
// whencePlotOutput().
//
// A value and a table come with their panels, so those open with no trip to
// the server. The panel of one row of a table, or of the point of a plot,
// opens at once with "Loading" and is filled when the server answers
// ("whence:group"). Rows always come from the server ("whence:rows").
// Every request goes through one input; see requests.R.

(function () {
  "use strict";

  var MINUS = String.fromCharCode(0x2212);
  var CLOSE = String.fromCharCode(0x00d7);
  var HINT = "Show where this number came from";
  var FOCUSABLE =
    'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), ' +
    'textarea:not([disabled]), [tabindex]:not([tabindex="-1"])';

  // The one input for requests to the server. See request_input in requests.R.
  var REQUEST = ".whence_request";

  // The suffix of the click input of a whence plot. See output-plot.R.
  var CLICK_SUFFIX = "_whence_click";

  // How long an unbound output waits before it tells the server that it is
  // gone. renderUI() unbinds an output and binds a new one with the same id
  // at once, and that must not count as gone.
  var GONE_DELAY = 500;

  // The state of each output, by output id: the latest data, and while a
  // panel is open, its elements, the element that opened it, and the row of
  // the table that it is for.
  var outputs = {};

  function request(id, action, extra) {
    var value = { id: id, action: action, at: Date.now() };
    Object.keys(extra || {}).forEach(function (key) {
      if (extra[key] !== null && extra[key] !== undefined) {
        value[key] = extra[key];
      }
    });
    Shiny.setInputValue(REQUEST, value, { priority: "event" });
  }

  function make(tag, attrs, text) {
    var node = document.createElement(tag);
    if (attrs) {
      Object.keys(attrs).forEach(function (key) {
        if (attrs[key] !== null && attrs[key] !== undefined) {
          node.setAttribute(key, attrs[key]);
        }
      });
    }
    if (text !== undefined && text !== null) {
      node.textContent = text;
    }
    return node;
  }

  function slotOf(el) {
    var slot = el.querySelector(".whence-slot");
    if (!slot) {
      slot = make("span", { class: "whence-slot" });
      el.appendChild(slot);
    }
    return slot;
  }

  function stateOf(id) {
    if (!outputs[id]) {
      outputs[id] = { data: null, panel: null, backdrop: null, trigger: null, row: null, goneTimer: null };
    }
    return outputs[id];
  }

  function labelOf(id) {
    var el = document.getElementById(id);
    var holder = el && el.closest(".whence-plot") ? el.closest(".whence-plot") : el;
    return holder ? holder.getAttribute("data-label") : null;
  }

  // Make an element act as a button that opens a panel.
  function asTrigger(node, open) {
    node.setAttribute("role", "button");
    node.setAttribute("tabindex", "0");
    node.setAttribute("aria-haspopup", "dialog");
    node.setAttribute("aria-expanded", "false");
    node.addEventListener("click", open);
    node.addEventListener("keydown", function (event) {
      if (event.key === "Enter" || event.key === " " || event.key === "Spacebar") {
        event.preventDefault();
        open();
      }
    });
    return node;
  }

  // --- A value ----------------------------------------------------------

  function renderValue(el, data) {
    var id = el.id;
    var state = stateOf(id);
    var restoreFocus = state.panel && state.panel.contains(document.activeElement);
    closePanel(id, false);
    cancelGone(state);
    state.data = data;

    var slot = slotOf(el);
    slot.textContent = "";
    if (!data) {
      return;
    }
    if (data.kind === "table") {
      renderTable(slot, id, data);
      return;
    }

    var hintId = id + "-whence-hint";
    var value = make("span", { class: "whence-value", "aria-describedby": hintId }, data.value);
    asTrigger(value, function () {
      openPanel(id, { data: data, trigger: value });
    });
    slot.appendChild(value);
    slot.appendChild(make("span", { id: hintId, class: "whence-sr" }, HINT));
    if (restoreFocus) {
      value.focus();
    }
  }

  // --- A table ----------------------------------------------------------

  function renderTable(container, id, data) {
    container.textContent = "";

    var wrap = make("div", { class: "whence-table-wrap" });
    var table = make("table", { class: "whence-table table table-sm" });
    var head = make("thead");
    var headRow = make("tr");
    data.columns.forEach(function (name) {
      headRow.appendChild(make("th", { scope: "col" }, name));
    });
    head.appendChild(headRow);
    table.appendChild(head);

    var body = make("tbody");
    (data.rows || []).forEach(function (cells, index) {
      var tr = make("tr");
      cells.forEach(function (text, col) {
        var td = make(col === 0 ? "th" : "td", col === 0 ? { scope: "row" } : null);
        if (col === 0 && data.explainable) {
          var trigger = make("span", {
            class: "whence-value",
            "aria-label": "Explain the row " + cells.join(", ")
          }, text);
          asTrigger(trigger, function () {
            openGroup(id, index + 1, trigger, null);
          });
          td.appendChild(trigger);
        } else {
          td.textContent = text;
        }
        tr.appendChild(td);
      });
      body.appendChild(tr);
    });
    table.appendChild(body);
    wrap.appendChild(table);
    container.appendChild(wrap);

    var foot = make("div", { class: "whence-table-foot" });
    if (data.shown < data.total) {
      foot.appendChild(make("span", { class: "whence-table-note" }, "The first " + data.shown + " of " +
        data.totalText + " rows are shown."));
    }
    if (!data.explainable && data.keysReason) {
      foot.appendChild(make("span", { class: "whence-table-note" }, data.keysReason));
    }
    var whole = make("button", { type: "button", class: "whence-table-explain" }, "Where these rows came from");
    whole.setAttribute("aria-haspopup", "dialog");
    whole.addEventListener("click", function () {
      openPanel(id, { data: data.panel, trigger: whole });
    });
    foot.appendChild(whole);
    container.appendChild(foot);
  }

  function openGroup(id, row, trigger, anchor) {
    openPanel(id, { data: null, trigger: trigger, anchor: anchor, row: row, group: true });
    request(id, "group", { row: row });
  }

  function onGroup(message) {
    var state = outputs[message.id];
    if (!state || !state.panel) {
      return;
    }
    if (state.row !== null && message.row !== undefined && message.row !== null && state.row !== message.row) {
      return;
    }
    if (!message.ok) {
      fillPanel(message.id, null, message.message);
      return;
    }
    state.row = message.row;
    fillPanel(message.id, message.panel);
  }

  // --- A plot -----------------------------------------------------------

  // A whence plot sends no click input of its own. Each click becomes a
  // "point" request, and the server finds the row under it.
  function onInputChanged(event) {
    if (typeof event.name !== "string" || event.name.slice(-CLICK_SUFFIX.length) !== CLICK_SUFFIX || !event.value) {
      return;
    }
    var id = event.name.slice(0, -CLICK_SUFFIX.length);
    var plot = document.getElementById(id);
    if (!plot || !plot.closest(".whence-plot")) {
      return;
    }
    event.preventDefault();
    var image = plot.querySelector("img") || plot;
    var rect = image.getBoundingClientRect();
    var css = event.value.coords_css || { x: 0, y: 0 };
    var anchor = { left: rect.left + css.x, bottom: rect.top + css.y };
    openPanel(id, { data: null, trigger: plot, anchor: anchor, row: null, group: true });
    request(id, "point", { click: event.value });
  }

  function onTable(message) {
    var container = document.getElementById(message.id + "-whence-table");
    if (container) {
      renderTable(container, message.id, message.table);
    }
  }

  // --- The panel --------------------------------------------------------

  // options: data (the panel, or null while it loads), trigger (the element
  // that gets the focus back), anchor (a point to put the panel under, in
  // place of the trigger), row (the row of a table, or null), group (TRUE
  // for the panel of a row of a table or a point of a plot).
  function openPanel(id, options) {
    var state = stateOf(id);
    if (state.panel) {
      closePanel(id, false);
    }
    var label = labelOf(id);
    var titleId = id + "-whence-title";

    var backdrop = make("div", { class: "whence-backdrop" });
    backdrop.addEventListener("click", function () {
      closePanel(id, true);
    });

    var panel = make("div", {
      class: "whence-panel",
      role: "dialog",
      "aria-modal": "true",
      "aria-labelledby": titleId,
      tabindex: "-1",
      "data-whence-for": id
    });

    var header = make("div", { class: "whence-header" });
    var title = options.group ? "Where this row came from" :
      (label ? "Where " + label + " came from" : "Where this number came from");
    header.appendChild(make("h2", { id: titleId, class: "whence-title" }, title));
    var close = make("button", { type: "button", class: "whence-close", "aria-label": "Close" }, CLOSE);
    close.addEventListener("click", function () {
      closePanel(id, true);
    });
    header.appendChild(close);
    panel.appendChild(header);
    panel.appendChild(make("div", { class: "whence-body" }));

    panel.addEventListener("keydown", function (event) {
      onPanelKey(id, event);
    });

    document.body.appendChild(backdrop);
    document.body.appendChild(panel);
    state.panel = panel;
    state.backdrop = backdrop;
    state.trigger = options.trigger || null;
    state.row = options.row === undefined ? null : options.row;

    if (state.trigger) {
      state.trigger.setAttribute("aria-expanded", "true");
    }
    fillPanel(id, options.data);
    place(panel, state.trigger, options.anchor);

    window.requestAnimationFrame(function () {
      panel.classList.add("whence-open");
      backdrop.classList.add("whence-open");
    });
    panel.focus();
    // A click on a plot is followed by its mouseup, which can move the focus
    // to the page. Focus the panel again once the click is over.
    window.setTimeout(function () {
      if (state.panel === panel && !panel.contains(document.activeElement)) {
        panel.focus();
      }
    }, 0);
  }

  // While a panel is open, Escape closes it and Tab goes back into it, also
  // when the focus has left the panel.
  function onDocumentKey(event) {
    var id = Object.keys(outputs).filter(function (key) {
      return outputs[key].panel;
    })[0];
    if (id === undefined) {
      return;
    }
    var panel = outputs[id].panel;
    if (panel.contains(document.activeElement)) {
      return;
    }
    if (event.key === "Escape" || event.key === "Esc") {
      event.preventDefault();
      closePanel(id, true);
    } else if (event.key === "Tab") {
      event.preventDefault();
      panel.focus();
    }
  }

  // Put the content in the panel: the data, "Loading" while it is null, or
  // a note when the server could not explain it.
  function fillPanel(id, data, note) {
    var state = outputs[id];
    var body = state.panel.querySelector(".whence-body");
    var refocus = state.panel.contains(document.activeElement) && document.activeElement !== state.panel;
    body.textContent = "";

    if (note) {
      body.removeAttribute("aria-busy");
      body.appendChild(make("p", { class: "whence-untraced" }, note));
      return;
    }
    if (!data) {
      body.setAttribute("aria-busy", "true");
      body.appendChild(make("p", { class: "whence-loading" }, "Loading..."));
      return;
    }
    body.removeAttribute("aria-busy");

    body.appendChild(make("p", { class: "whence-summary" }, data.value));
    if (data.rowText) {
      body.appendChild(make("p", { class: "whence-row-text" }, data.rowText));
    }
    body.appendChild(make("h3", { class: "whence-section" }, "Where the rows went"));
    body.appendChild(waterfall(id, data));
    body.appendChild(codeSection(data));
    body.appendChild(rowsSection(id, data));
    if (refocus) {
      state.panel.focus();
    }
  }

  function waterfall(id, data) {
    if (!data.traced) {
      return make("p", { class: "whence-untraced" }, data.reason || "Pipeline not traced.");
    }
    var wrap = make("div", { class: "whence-waterfall-wrap" });
    var list = make("dl", { class: "whence-waterfall" });
    (data.stages || []).forEach(function (stage) {
      var step = make("div", { class: "whence-step whence-kind-" + stage.kind });
      step.appendChild(make("dt", null, stage.label));

      var dd = make("dd");
      if (stage.nText !== undefined) {
        var unit = stage.n === 1 ? " row" : " rows";
        dd.appendChild(make("span", { class: "whence-n" }, stage.nText + unit));
      }
      if (stage.groups !== undefined && (stage.kind === "group_by" || stage.kind === "ungroup")) {
        dd.appendChild(make("span", { class: "whence-groups" }, stage.groups + " groups"));
      }
      if (stage.delta !== undefined) {
        dd.appendChild(deltaElement(id, stage));
      }
      step.appendChild(dd);
      list.appendChild(step);
    });
    wrap.appendChild(list);
    // The chain stopped at a call that whence does not know, such as a join.
    if (data.reason) {
      wrap.appendChild(make("p", { class: "whence-untraced" }, data.reason));
    }
    return wrap;
  }

  // A count of removed rows. When the server can find those rows, it is a
  // button that shows them.
  function deltaElement(id, stage) {
    var removed = stage.delta < 0;
    var noun = Math.abs(stage.delta) === 1 ? " row" : " rows";
    var text = (removed ? MINUS : "+") + stage.deltaText + noun +
      (stage.percentText ? " (" + stage.percentText + ")" : "");
    var cls = removed ? "whence-delta" : "whence-delta whence-delta-added";
    if (!removed || !stage.removable) {
      return make("span", { class: cls }, text);
    }
    var button = make("button", {
      type: "button",
      class: cls + " whence-delta-button",
      "aria-label": "Show the " + stage.deltaText + noun + " that " + stage.label + " removed"
    }, text);
    button.addEventListener("click", function () {
      var state = outputs[id];
      var region = state.panel.querySelector(".whence-rows");
      region.setAttribute("aria-busy", "true");
      request(id, "removed", { step: stage.step, row: state.row });
    });
    return button;
  }

  function codeSection(data) {
    var section = make("div", { class: "whence-reproduce" });
    var head = make("div", { class: "whence-section-head" });
    head.appendChild(make("h3", { class: "whence-section" }, "Reproduce"));
    var copy = make("button", { type: "button", class: "whence-copy btn btn-sm btn-outline-secondary" }, "Copy");
    var status = make("span", { class: "whence-sr", role: "status" });
    copy.addEventListener("click", function () {
      copyText(data.code, copy, function (ok) {
        status.textContent = ok ? "Code copied." : "Could not copy the code.";
        copy.textContent = ok ? "Copied" : "Copy";
      });
    });
    head.appendChild(copy);
    head.appendChild(status);
    section.appendChild(head);

    var pre = make("pre", { class: "whence-code" });
    pre.appendChild(make("code", null, data.code));
    section.appendChild(pre);
    return section;
  }

  function rowsSection(id, data) {
    var section = make("div", { class: "whence-rows-section" });
    var region = make("div", { class: "whence-rows", "aria-live": "polite" });

    if (data.rowsAvailable) {
      var button = make(
        "button",
        { type: "button", class: "whence-rows-button btn btn-sm btn-outline-primary" },
        "View the rows"
      );
      button.addEventListener("click", function () {
        // aria-disabled and not disabled: a disabled button loses the focus
        // to the page, outside the dialog. showRows() moves the focus on.
        if (button.getAttribute("aria-disabled") === "true") {
          return;
        }
        button.setAttribute("aria-disabled", "true");
        button.textContent = "Loading the rows...";
        region.setAttribute("aria-busy", "true");
        request(id, "rows", { row: outputs[id].row });
      });
      section.appendChild(button);
    } else if (data.rowsReason) {
      region.appendChild(make("p", { class: "whence-rows-note" }, data.rowsReason));
    }

    section.appendChild(region);
    return section;
  }

  // Put the panel below its trigger, or below the point of a click. On a
  // narrow screen the CSS makes it a sheet at the bottom and ignores these
  // positions.
  function place(panel, trigger, anchor) {
    var rect = anchor || (trigger ? trigger.getBoundingClientRect() : null);
    if (!rect) {
      return;
    }
    var margin = 8;
    var width = panel.offsetWidth;
    var left = rect.left + window.scrollX;
    var maxLeft = window.scrollX + document.documentElement.clientWidth - width - margin;
    left = Math.max(window.scrollX + margin, Math.min(left, maxLeft));
    panel.style.setProperty("--whence-top", rect.bottom + window.scrollY + 6 + "px");
    panel.style.setProperty("--whence-left", left + "px");
  }

  function onPanelKey(id, event) {
    var panel = stateOf(id).panel;
    if (!panel) {
      return;
    }
    if (event.key === "Escape" || event.key === "Esc") {
      event.preventDefault();
      event.stopPropagation();
      closePanel(id, true);
      return;
    }
    if (event.key !== "Tab") {
      return;
    }
    // Keep the focus in the panel while it is open.
    var items = Array.prototype.slice.call(panel.querySelectorAll(FOCUSABLE)).filter(function (node) {
      return node.offsetParent !== null || node === document.activeElement;
    });
    if (items.length === 0) {
      event.preventDefault();
      panel.focus();
      return;
    }
    var first = items[0];
    var last = items[items.length - 1];
    var active = document.activeElement;
    if (event.shiftKey && (active === first || active === panel)) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && active === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function closePanel(id, restoreFocus) {
    var state = outputs[id];
    if (!state || !state.panel) {
      return;
    }
    state.panel.remove();
    state.backdrop.remove();
    state.panel = null;
    state.backdrop = null;

    var trigger = state.trigger;
    state.trigger = null;
    state.row = null;
    if (trigger && document.body.contains(trigger)) {
      trigger.setAttribute("aria-expanded", "false");
      if (restoreFocus) {
        if (!trigger.hasAttribute("tabindex")) {
          trigger.setAttribute("tabindex", "-1");
        }
        trigger.focus();
      }
    }
  }

  function copyText(text, button, done) {
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(
        function () {
          done(true);
        },
        function () {
          done(fallbackCopy(text, button));
        }
      );
      return;
    }
    done(fallbackCopy(text, button));
  }

  function fallbackCopy(text, button) {
    var area = make("textarea", { class: "whence-sr", "aria-hidden": "true" });
    area.value = text;
    document.body.appendChild(area);
    area.select();
    var ok = false;
    try {
      ok = document.execCommand("copy");
    } catch (e) {
      ok = false;
    }
    area.remove();
    button.focus();
    return ok;
  }

  // --- The rows ---------------------------------------------------------

  function showRows(message) {
    var state = outputs[message.id];
    if (!state || !state.panel) {
      return;
    }
    if (message.row !== undefined && message.row !== null && state.row !== message.row) {
      return;
    }
    var panel = state.panel;
    var region = panel.querySelector(".whence-rows");
    if (!region) {
      return;
    }
    var button = panel.querySelector(".whence-rows-button");
    var active = document.activeElement;
    var refocus = active && (active === button || active.classList.contains("whence-delta-button"));
    if (button && !message.title) {
      // "View the rows" has done its job.
      button.remove();
    } else if (button) {
      button.removeAttribute("aria-disabled");
      button.textContent = "View the rows";
    }
    region.removeAttribute("aria-busy");
    region.textContent = "";

    if (!message.ok) {
      var note = make("p", { class: "whence-rows-note", tabindex: "-1" }, message.message);
      region.appendChild(note);
      if (refocus) {
        note.focus();
      }
      return;
    }

    var noun = message.total === 1 ? " row" : " rows";
    var heading = message.totalText + noun + (message.title ? " " + message.title : "");
    if (message.shown < message.total) {
      heading += " (the first " + message.shownText + " are shown)";
    }
    var title = make("h3", { class: "whence-section", tabindex: "-1" }, heading);
    region.appendChild(title);
    if (refocus) {
      title.focus();
    }
    var table = make("div", { class: "whence-rows-content" });
    region.appendChild(table);

    // The table is an htmlwidget, and its scripts come with the message.
    // renderContentAsync() waits until those scripts have loaded.
    // renderContent() only adds the script tags, so a widget drawn right
    // after it finds no HTMLWidgets yet on a slow page, such as shinylive.
    var content = { html: message.html, deps: message.deps };
    var rendered = Shiny.renderContentAsync
      ? Shiny.renderContentAsync(table, content)
      : Shiny.renderContent(table, content);
    Promise.resolve(rendered).then(function () {
      if (window.HTMLWidgets && window.HTMLWidgets.staticRender) {
        window.HTMLWidgets.staticRender();
      }
    });
  }

  // --- Clean-up when an output is removed ----------------------------------

  // The panel is in document.body, so it does not go with its output. When
  // Shiny unbinds an output, the panel closes at once. If no output with the
  // same id is bound again soon, the state here is dropped and the server is
  // told, so that it drops the trace and its data.
  function onUnbound(event) {
    var el = event.target;
    if (event.bindingType !== "output" || !el.classList) {
      return;
    }
    var ours = el.classList.contains("whence-output") || (el.closest && el.closest(".whence-plot"));
    if (!ours) {
      return;
    }
    var id = el.id;
    var state = outputs[id];
    if (!state) {
      // A plot whose panel was never opened still has a trace on the server.
      state = stateOf(id);
    }
    closePanel(id, false);
    cancelGone(state);
    state.goneTimer = window.setTimeout(function () {
      state.goneTimer = null;
      var again = document.getElementById(id);
      if (again && again.classList.contains("shiny-bound-output")) {
        return;
      }
      delete outputs[id];
      request(id, "gone");
    }, GONE_DELAY);
  }

  function cancelGone(state) {
    if (state.goneTimer) {
      window.clearTimeout(state.goneTimer);
      state.goneTimer = null;
    }
  }

  // --- Registration -----------------------------------------------------

  function register() {
    var binding = new Shiny.OutputBinding();
    binding.find = function (scope) {
      return $(scope).find(".whence-output");
    };
    binding.renderValue = renderValue;
    binding.renderError = function (el, err) {
      closePanel(el.id, false);
      el.classList.add("shiny-output-error");
      if (err && err.type) {
        el.classList.add("shiny-output-error-" + err.type);
      }
      slotOf(el).textContent = err && err.message ? err.message : "";
    };
    binding.clearError = function (el) {
      Array.prototype.slice.call(el.classList).forEach(function (name) {
        if (name.indexOf("shiny-output-error") === 0) {
          el.classList.remove(name);
        }
      });
    };
    Shiny.outputBindings.register(binding, "whence.value");
    Shiny.addCustomMessageHandler("whence:rows", showRows);
    Shiny.addCustomMessageHandler("whence:group", onGroup);
    Shiny.addCustomMessageHandler("whence:table", onTable);
    $(document).on("shiny:unbound", onUnbound);
    $(document).on("shiny:inputchanged", onInputChanged);
    document.addEventListener("keydown", onDocumentKey);
  }

  if (window.Shiny && window.Shiny.OutputBinding) {
    register();
  } else {
    document.addEventListener("DOMContentLoaded", register);
  }
})();

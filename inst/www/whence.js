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

  // The input that tells the app which panel is open. See opened.R.
  var OPENED = ".whence_opened";

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

  // A pending open and the visible preview share one state, even across outputs.
  var preview = null;
  var previewViewport = window.matchMedia("(min-width: 576px)");
  var nextPreviewRequest = 0;
  var restoringPanelFocus = false;
  var AGGREGATES = ["summarise", "count", "tally"];
  var NO_CUT = AGGREGATES.concat([
    "source", "group_by", "ungroup", "collect", "compute", "as_tibble", "as.data.frame", "distinct"
  ]);

  function request(id, action, extra) {
    var value = { id: id, action: action, at: Date.now() };
    Object.keys(extra || {}).forEach(function (key) {
      if (extra[key] !== null && extra[key] !== undefined) {
        value[key] = extra[key];
      }
    });
    Shiny.setInputValue(REQUEST, value, { priority: "event" });
  }

  // Tell the app which panel is open, for whence_opened(). The panel does
  // not wait for this.
  function notifyOpened(id, row, open) {
    var value = { id: id, open: open, at: Date.now() };
    if (row !== null && row !== undefined) {
      value.row = row;
    }
    Shiny.setInputValue(OPENED, value, { priority: "event" });
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

  function appendMark(node) {
    var mark = make("span", { class: "whence-mark", "aria-hidden": "true" });
    mark.appendChild(make("i"));
    mark.appendChild(make("i"));
    mark.appendChild(make("i"));
    node.appendChild(mark);
  }

  // --- A value ----------------------------------------------------------

  function renderValue(el, data) {
    var id = el.id;
    var state = stateOf(id);
    var restoreFocus = state.panel && state.panel.contains(document.activeElement);
    closePreview(id);
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
    if (el.getAttribute("data-whence-marker") === "mark") {
      appendMark(value);
    }
    asTrigger(value, function () {
      openPanel(id, { data: data, trigger: value });
    });
    bindPreview(id, value, function () {
      return { stages: data.stages, panel: data, untraced: !data.traced, reason: data.reason };
    });
    slot.appendChild(value);
    slot.appendChild(make("span", { id: hintId, class: "whence-sr" }, HINT));
    if (restoreFocus) {
      restorePanelFocus(value);
    }
  }

  // --- A table ----------------------------------------------------------

  function renderTable(container, id, data) {
    closePreview(id);
    stateOf(id).previewGroups = {};
    container.textContent = "";

    var holder = container.closest("[data-whence-marker]");
    var marked = holder && holder.getAttribute("data-whence-marker") === "mark";
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
          if (marked) {
            appendMark(trigger);
          }
          asTrigger(trigger, function () {
            openGroup(id, index + 1, trigger, null);
          });
          bindPreview(id, trigger, function () {
            return rowPreview(data, index + 1);
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

  function rowPreview(data, row) {
    var grid = data.hover;
    var counts = grid && grid.counts && grid.counts[row - 1];
    if (counts) {
      var stages = grid.steps.map(function (step, i) {
        return { label: step.label, kind: step.kind, n: counts[i] };
      });
      if (stages.length && Number.isFinite(stages[0].n)) {
        return { stages: stages, row: row, subject: "row" };
      }
    }
    return {
      stages: data.panel.stages,
      row: row,
      subject: "table",
      reason: (grid && grid.reason) || "Counts for this row are not known: some steps have no group counts."
    };
  }

  // --- Hover previews ----------------------------------------------------

  function summarise(stages) {
    var source = stages[0] || {};
    var end = stages.length;
    stages.forEach(function (stage, index) {
      if (AGGREGATES.indexOf(stage.kind) !== -1) {
        end = index;
      }
    });
    // The rows that enter the final summary are the rows that make the result.
    var to = (stages[Math.max(0, end - 1)] || {}).n;
    var cut = null;
    for (var i = 1; i < end; i++) {
      var stage = stages[i];
      var before = stages[i - 1].n;
      if (NO_CUT.indexOf(stage.kind) !== -1) {
        continue;
      }
      var removed = Number.isFinite(stage.delta) ? -stage.delta :
        (Number.isFinite(before) && Number.isFinite(stage.n) ? before - stage.n : 0);
      if (removed > 0 && (!cut || removed > cut.removed)) {
        cut = { label: stage.label, removed: removed, percent: stage.percentText || previewPercent(removed, before) };
      }
    }
    return { from: source.n, to: to, source: source.label, steps: Math.max(0, stages.length - 1), cut: cut };
  }

  function previewPercent(count, total) {
    if (!Number.isFinite(total) || total === 0) {
      return count === 0 ? "0%" : "";
    }
    var percent = 100 * count / total;
    if (percent > 0 && percent < 1) {
      return "<1%";
    }
    return (percent < 100 ? Math.min(99, Math.round(percent)) : Math.round(percent)) + "%";
  }

  function previewContent(style, summary, stages) {
    var hint = make("p", { class: "whence-preview-hint" }, "Click to see every step");
    if (style === "panel") {
      var dialog = panelFrame(summary.id, summary.row !== undefined, function () {
        closePreview(summary.id);
      }, false);
      fillPanel(summary.id, summary.panel, summary.note, { node: dialog, trigger: summary.trigger });
      dialog.appendChild(hint);
      return dialog;
    }

    var node = make("div", { class: "whence-preview", role: "tooltip" });
    var known = Number.isFinite(summary.from) && Number.isFinite(summary.to);
    if (summary.reason) {
      node.appendChild(make("p", { class: "whence-preview-note" }, summary.reason));
    }
    if (!summary.untraced && style === "waterfall") {
      stages.forEach(function (stage) {
        var counted = Number.isFinite(stage.n);
        var amount = counted ? stage.n.toLocaleString("en-GB") : "Unknown";
        var step = make("div", { class: "whence-preview-step" });
        step.appendChild(make("span", null, stage.label));
        step.appendChild(make("span", null, amount));
        node.appendChild(step);
        var track = make("div", { class: "whence-preview-track", "aria-hidden": "true" });
        var bar = make("div", { class: "whence-preview-bar" });
        bar.style.width = (counted && summary.from > 0 ? 100 * stage.n / summary.from : 0) + "%";
        track.appendChild(bar);
        node.appendChild(track);
      });
    } else if (!summary.untraced && known) {
      var from = summary.from.toLocaleString("en-GB");
      var to = summary.to.toLocaleString("en-GB");
      if (style === "line") {
        node.appendChild(make("p", null, "From " + from + " rows of " + summary.source + " to " + to +
          ", in " + summary.steps + (summary.steps === 1 ? " step." : " steps.")));
      } else {
        node.appendChild(make("p", null, to + " of " + from + " rows (" +
          previewPercent(summary.to, summary.from) + ") make this " + summary.subject + "."));
        var cut = summary.cut;
        if (cut) {
          node.appendChild(make("p", null, "Biggest cut: " + cut.label));
          node.appendChild(make("p", null, "-" + cut.removed.toLocaleString("en-GB") + " rows" +
            (cut.percent ? " (" + cut.percent + ")" : "")));
        } else {
          node.appendChild(make("p", null, "No step removed rows."));
        }
      }
    } else if (!summary.reason) {
      node.appendChild(make("p", null, "Row counts are not available."));
    }
    node.appendChild(hint);
    return node;
  }

  function openPreview(id, trigger, content) {
    if (!preview || preview.id !== id || preview.trigger !== trigger || preview.node) {
      closePreview();
      preview = { id: id, trigger: trigger, node: null, openTimer: null, closeTimer: null };
    }
    var active = preview;
    active.node = content;
    if (content.getAttribute("role") === "tooltip") {
      content.id = id + "-whence-preview";
      active.description = trigger.getAttribute("aria-describedby");
      trigger.setAttribute("aria-describedby", content.id);
    } else {
      content.addEventListener("click", pinPreview, true);
      content.addEventListener("focusout", function (event) {
        if (preview === active && !content.contains(event.relatedTarget) &&
            (event.relatedTarget || !active.overPreview)) {
          closePreview(id);
        }
      });
    }
    content.addEventListener("pointerover", function (event) {
      if (event.pointerType !== "touch" && preview === active) {
        active.overPreview = true;
        window.clearTimeout(active.closeTimer);
      }
    });
    content.addEventListener("pointerout", function (event) {
      if (event.pointerType !== "touch" && preview === active && !content.contains(event.relatedTarget)) {
        active.overPreview = false;
        if (!trigger.contains(event.relatedTarget)) {
          leavePreview(active);
        }
      }
    });
    document.body.appendChild(content);
    place(content, trigger);
  }

  function closePreview(id) {
    if (!preview || (id !== undefined && preview.id !== id)) {
      return;
    }
    var active = preview;
    preview = null;
    window.clearTimeout(active.openTimer);
    window.clearTimeout(active.closeTimer);
    if (active.node) {
      if (active.description === null) {
        active.trigger.removeAttribute("aria-describedby");
      } else if (active.description !== undefined) {
        active.trigger.setAttribute("aria-describedby", active.description);
      }
      active.node.remove();
    }
  }

  function leavePreview(active) {
    if (active.overPreview) {
      return;
    }
    if (!active.node) {
      closePreview(active.id);
      return;
    }
    window.clearTimeout(active.closeTimer);
    active.closeTimer = window.setTimeout(function () {
      if (preview === active) {
        closePreview(active.id);
      }
    }, 200);
  }

  function bindPreview(id, trigger, getStages) {
    var pointerFocus = false;
    function start(focus) {
      var holder = trigger.closest("[data-whence-hover]");
      var style = holder && holder.getAttribute("data-whence-hover");
      if (!style || style === "none" || !previewViewport.matches ||
          Object.keys(outputs).some(function (key) { return outputs[key].panel; })) {
        return;
      }
      if (preview && preview.trigger === trigger) {
        window.clearTimeout(preview.closeTimer);
        if (preview.node || !focus) {
          return;
        }
      }
      closePreview();
      var active = { id: id, trigger: trigger, node: null, openTimer: null, closeTimer: null };
      preview = active;
      var show = function () {
        if (preview !== active || !trigger.isConnected || !previewViewport.matches) {
          return;
        }
        var data = getStages();
        var stages = data.stages || [];
        if (style === "panel") {
          showPanelPreview(active, data);
        } else {
          var summary = summarise(stages);
          summary.reason = data.reason;
          summary.untraced = data.untraced;
          summary.subject = data.subject || "value";
          openPreview(id, trigger, previewContent(style, summary, stages));
        }
      };
      if (focus) {
        show();
      } else {
        active.openTimer = window.setTimeout(show, 300);
      }
    }

    trigger.addEventListener("pointerover", function (event) {
      if (event.pointerType !== "touch" && !trigger.contains(event.relatedTarget)) {
        start(false);
      }
    });
    trigger.addEventListener("pointerout", function (event) {
      if (event.pointerType !== "touch" && preview && preview.trigger === trigger &&
          !trigger.contains(event.relatedTarget)) {
        leavePreview(preview);
      }
    });
    trigger.addEventListener("pointerdown", function () {
      // Focus from a press should keep the pointer delay, or stay off on touch.
      pointerFocus = true;
    });
    trigger.addEventListener("pointercancel", function () {
      pointerFocus = false;
    });
    trigger.addEventListener("focus", function () {
      if (!pointerFocus && !restoringPanelFocus) {
        start(true);
      }
      pointerFocus = false;
    });
    trigger.addEventListener("blur", function (event) {
      pointerFocus = false;
      if (preview && preview.trigger === trigger) {
        var dialog = preview.node && preview.node.getAttribute("role") === "dialog";
        // A dialog can take focus when the pointer enters one of its controls.
        if (!dialog || (!preview.node.contains(event.relatedTarget) && !(preview.overPreview && !event.relatedTarget))) {
          closePreview(id);
        }
      }
    });
  }

  function showPanelPreview(active, data) {
    var entry;
    var needsReply = false;
    if (data.row !== undefined) {
      var groups = outputs[active.id].previewGroups;
      entry = groups[data.row];
      if (!entry) {
        needsReply = true;
        groups[data.row] = entry = { token: String(++nextPreviewRequest), reply: null };
      }
      active.entry = entry;
      active.row = data.row;
    }
    var reply = entry && entry.reply;
    var summary = {
      id: active.id,
      trigger: active.trigger,
      row: active.row,
      panel: entry ? reply && reply.panel : data.panel,
      note: reply && reply.message
    };
    openPreview(active.id, active.trigger, previewContent("panel", summary, []));
    if (needsReply) {
      request(active.id, "group", { row: active.row, preview: entry.token });
    }
  }

  // Reuse the normal trigger. If a preview control was clicked, run its
  // matching action in the pinned panel so rows and copy work there.
  function pinPreview(event) {
    var active = preview;
    var button = event.target.closest("button");
    if (!active || (button && button.classList.contains("whence-close"))) {
      return;
    }
    var index = Array.prototype.indexOf.call(active.node.querySelectorAll("button"), button);
    event.preventDefault();
    event.stopImmediatePropagation();
    active.trigger.click();
    var panel = outputs[active.id].panel;
    if (panel && index !== -1) {
      var control = panel.querySelectorAll("button")[index];
      if (control) {
        control.focus();
        control.click();
      }
    }
  }

  function openGroup(id, row, trigger, anchor) {
    var entry = preview && preview.trigger === trigger && preview.entry;
    var reply = entry && entry.reply;
    openPanel(id, {
      data: reply ? reply.panel : null,
      note: reply && reply.message,
      trigger: trigger,
      anchor: anchor,
      row: row,
      group: true,
      pendingPreview: entry && !reply ? entry.token : null
    });
    if (!entry) {
      request(id, "group", { row: row });
    }
  }

  function onGroup(message) {
    var state = outputs[message.id];
    if (message.preview !== undefined) {
      var entry = state && state.previewGroups && state.previewGroups[message.row];
      if (!entry || entry.token !== message.preview) {
        return;
      }
      entry.reply = message;
      if (!message.ok) {
        delete state.previewGroups[message.row];
      }
      if (preview && preview.entry === entry) {
        fillPanel(message.id, message.panel, message.message, preview);
      }
      // A hover reply can also finish a panel pinned while it was loading.
      if (state.pendingPreview !== entry.token) {
        return;
      }
      state.pendingPreview = null;
    } else if (state && state.pendingPreview) {
      return;
    }
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
    var known = state.row !== null;
    state.row = message.row;
    fillPanel(message.id, message.panel);
    if (!known) {
      notifyOpened(message.id, state.row, true);
    }
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
    // The plot was drawn again, so the server holds a new table. An open
    // panel belongs to the old one, and its row number can point to another
    // group now: close it, as renderValue() does for a value or a table.
    var state = outputs[message.id];
    if (state && state.panel) {
      closePanel(message.id, state.panel.contains(document.activeElement));
    }
    var container = document.getElementById(message.id + "-whence-table");
    if (container) {
      renderTable(container, message.id, message.table);
    }
  }

  // --- The panel --------------------------------------------------------

  function panelFrame(id, group, close, modal) {
    var label = labelOf(id);
    var titleId = id + (modal ? "-whence-title" : "-whence-preview-title");
    var panel = make("div", {
      class: modal ? "whence-panel" : "whence-preview whence-preview-panel",
      role: "dialog",
      "aria-modal": modal ? "true" : null,
      "aria-labelledby": titleId,
      tabindex: "-1",
      "data-whence-for": id
    });
    var header = make("div", { class: "whence-header" });
    var title = group ? "Where this row came from" :
      (label ? "Where " + label + " came from" : "Where this number came from");
    header.appendChild(make("h2", { id: titleId, class: "whence-title" }, title));
    var button = make("button", { type: "button", class: "whence-close", "aria-label": "Close" }, CLOSE);
    button.addEventListener("click", close);
    header.appendChild(button);
    panel.appendChild(header);
    panel.appendChild(make("div", { class: "whence-body" }));
    return panel;
  }

  // options: data (the panel, or null while it loads), trigger (the element
  // that gets the focus back), anchor (a point to put the panel under, in
  // place of the trigger), row (the row of a table, or null), group (TRUE
  // for the panel of a row of a table or a point of a plot).
  function openPanel(id, options) {
    closePreview();
    var state = stateOf(id);
    if (state.panel) {
      closePanel(id, false);
    }
    var backdrop = make("div", { class: "whence-backdrop" });
    backdrop.addEventListener("click", function () {
      closePanel(id, true);
    });

    var panel = panelFrame(id, options.group, function () {
      closePanel(id, true);
    }, true);

    panel.addEventListener("keydown", function (event) {
      onPanelKey(id, event);
    });

    document.body.appendChild(backdrop);
    document.body.appendChild(panel);
    state.panel = panel;
    state.backdrop = backdrop;
    state.trigger = options.trigger || null;
    state.row = options.row === undefined ? null : options.row;
    state.anchor = options.anchor || null;
    state.pendingPreview = options.pendingPreview || null;

    if (state.trigger) {
      state.trigger.setAttribute("aria-expanded", "true");
    }
    fillPanel(id, options.data, options.note);
    place(panel, state.trigger, options.anchor);
    // The panel of a plot click does not know its row yet; onGroup() tells
    // the app when it does.
    if (!options.group || state.row !== null) {
      notifyOpened(id, state.row, true);
    }

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
    if (preview && (event.key === "Escape" || event.key === "Esc")) {
      event.preventDefault();
      var trigger = preview.node && preview.node.contains(document.activeElement) ? preview.trigger : null;
      closePreview();
      if (trigger) {
        restorePanelFocus(trigger);
      }
      return;
    }
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
  function fillPanel(id, data, note, hover) {
    var state = outputs[id];
    var panel = hover ? hover.node : state.panel;
    var body = panel.querySelector(".whence-body");
    var refocus = panel.contains(document.activeElement) && document.activeElement !== panel;
    body.textContent = "";

    if (note) {
      body.removeAttribute("aria-busy");
      body.appendChild(make("p", { class: "whence-untraced" }, note));
      if (hover && panel.isConnected) {
        place(panel, hover.trigger);
      }
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
    // The panel grew from "Loading", so it may need a new place.
    if (panel.isConnected) {
      place(panel, hover ? hover.trigger : state.trigger, hover ? null : state.anchor);
    }
    if (refocus) {
      panel.focus();
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
      // The number of the step, as whence_rows(step = ) takes it. CSS shows
      // it before the label.
      step.appendChild(make("dt", { "data-step": stage.step }, stage.label));

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
      if (stage.added > 0) {
        var addedNoun = stage.added === 1 ? " row" : " rows";
        var addedLabel = stage.addedText + addedNoun +
          " added because a row matched more than once";
        dd.appendChild(make("span", {
          class: "whence-delta whence-delta-added",
          title: addedLabel,
          "aria-label": addedLabel
        }, "+" + stage.addedText + addedNoun));
      }
      step.appendChild(dd);
      list.appendChild(step);
    });
    wrap.appendChild(list);
    // The chain stopped at a call that whence does not know, such as full_join().
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

  // Put the panel below its trigger, or below the point of a click. When
  // there is no room below and more room above, it goes above. On a narrow
  // screen the CSS makes it a sheet at the bottom and ignores these
  // positions.
  function place(panel, trigger, anchor) {
    var rect = anchor || (trigger ? trigger.getBoundingClientRect() : null);
    if (!rect) {
      return;
    }
    var margin = 8;
    var hover = panel.classList.contains("whence-preview");
    if (hover) {
      panel.style.removeProperty("--whence-preview-height");
    }
    var width = panel.offsetWidth;
    var height = panel.offsetHeight;
    var view = document.documentElement.clientHeight;
    var left = rect.left + window.scrollX;
    var maxLeft = window.scrollX + document.documentElement.clientWidth - width - margin;
    left = Math.max(window.scrollX + margin, Math.min(left, maxLeft));
    var top = rect.bottom + 6;
    var rectTop = rect.top === undefined ? rect.bottom : rect.top;
    if (hover) {
      // A tall preview scrolls within the available space so the trigger
      // stays clear and the pointer can get back to it.
      var below = view - margin - top;
      var above = rectTop - 6 - margin;
      var flip = height > below && above > below;
      panel.style.setProperty("--whence-preview-height", Math.max(0, flip ? above : below) + "px");
      if (flip) {
        top = rectTop - 6 - panel.offsetHeight;
      }
    } else if (top + height > view - margin && rectTop - 6 - height > view - top - height) {
      top = Math.max(margin, rectTop - 6 - height);
    }
    panel.style.setProperty("--whence-top", top + window.scrollY + "px");
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
    notifyOpened(id, null, false);

    var trigger = state.trigger;
    state.trigger = null;
    state.row = null;
    state.pendingPreview = null;
    if (trigger && document.body.contains(trigger)) {
      trigger.setAttribute("aria-expanded", "false");
      if (restoreFocus) {
        if (!trigger.hasAttribute("tabindex")) {
          trigger.setAttribute("tabindex", "-1");
        }
        restorePanelFocus(trigger);
      }
    }
  }

  function restorePanelFocus(trigger) {
    restoringPanelFocus = true;
    try {
      trigger.focus();
    } finally {
      restoringPanelFocus = false;
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
    closePreview(id);
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

  // "Show the data" opens its table below the plot, often below the fold, and
  // then it looks as if nothing happened. Bring the table into view. The
  // toggle event does not bubble, so it is caught on the way down.
  function onPlotDataToggle(event) {
    var details = event.target;
    if (!details.classList || !details.classList.contains("whence-plot-data") || !details.open) {
      return;
    }
    details.scrollIntoView({ block: "nearest", behavior: "smooth" });
  }

  function register() {
    var binding = new Shiny.OutputBinding();
    binding.find = function (scope) {
      return $(scope).find(".whence-output");
    };
    binding.renderValue = renderValue;
    binding.renderError = function (el, err) {
      closePreview(el.id);
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
    document.addEventListener("toggle", onPlotDataToggle, true);
    window.addEventListener("resize", function () {
      if (!previewViewport.matches) {
        closePreview();
      } else if (preview && preview.node) {
        place(preview.node, preview.trigger);
      }
    });
  }

  if (window.Shiny && window.Shiny.OutputBinding) {
    register();
  } else {
    document.addEventListener("DOMContentLoaded", register);
  }
})();

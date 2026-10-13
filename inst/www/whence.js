// whence.js - the browser side of explainableOutput().
//
// The render sends the value, the waterfall and the code together. The panel
// thus opens with no trip to the server. Only "View the rows" sends an input
// to the server, and the server answers with the "whence:rows" message.

(function () {
  "use strict";

  var MINUS = String.fromCharCode(0x2212);
  var CLOSE = String.fromCharCode(0x00d7);
  var HINT = "Show where this number came from";
  var FOCUSABLE =
    'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), ' +
    'textarea:not([disabled]), [tabindex]:not([tabindex="-1"])';

  // The state of each output, by output id: the latest data and, while the
  // panel is open, its elements.
  var outputs = {};

  // The one input for requests to the server. See request_input in output.R.
  var REQUEST = ".whence_request";

  // How long an unbound output waits before it tells the server that it is
  // gone. renderUI() unbinds an output and binds a new one with the same id
  // at once, and that must not count as gone.
  var GONE_DELAY = 500;

  function request(id, action) {
    Shiny.setInputValue(REQUEST, { id: id, action: action, at: Date.now() }, { priority: "event" });
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
      outputs[id] = { data: null, panel: null, backdrop: null, goneTimer: null };
    }
    return outputs[id];
  }

  // --- The value --------------------------------------------------------

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

    var hintId = id + "-whence-hint";
    var value = make(
      "span",
      {
        class: "whence-value",
        role: "button",
        tabindex: "0",
        "aria-haspopup": "dialog",
        "aria-expanded": "false",
        "aria-describedby": hintId
      },
      data.value
    );
    value.addEventListener("click", function () {
      openPanel(id);
    });
    value.addEventListener("keydown", function (event) {
      if (event.key === "Enter" || event.key === " " || event.key === "Spacebar") {
        event.preventDefault();
        openPanel(id);
      }
    });

    slot.appendChild(value);
    slot.appendChild(make("span", { id: hintId, class: "whence-sr" }, HINT));
    if (restoreFocus) {
      value.focus();
    }
  }

  function triggerOf(id) {
    var el = document.getElementById(id);
    return el ? el.querySelector(".whence-value") : null;
  }

  // --- The panel --------------------------------------------------------

  function openPanel(id) {
    var state = stateOf(id);
    if (!state.data || state.panel) {
      return;
    }
    var data = state.data;
    var output = document.getElementById(id);
    var label = output ? output.getAttribute("data-label") : null;
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
    header.appendChild(
      make(
        "h2",
        { id: titleId, class: "whence-title" },
        label ? "Where " + label + " came from" : "Where this number came from"
      )
    );
    var close = make("button", { type: "button", class: "whence-close", "aria-label": "Close" }, CLOSE);
    close.addEventListener("click", function () {
      closePanel(id, true);
    });
    header.appendChild(close);
    panel.appendChild(header);

    panel.appendChild(make("p", { class: "whence-summary" }, data.value));

    panel.appendChild(make("h3", { class: "whence-section" }, "Where the rows went"));
    panel.appendChild(waterfall(data));

    panel.appendChild(codeSection(data));
    panel.appendChild(rowsSection(id, data));

    panel.addEventListener("keydown", function (event) {
      onPanelKey(id, event);
    });

    document.body.appendChild(backdrop);
    document.body.appendChild(panel);
    state.panel = panel;
    state.backdrop = backdrop;

    var trigger = triggerOf(id);
    if (trigger) {
      trigger.setAttribute("aria-expanded", "true");
    }
    place(panel, trigger);

    window.requestAnimationFrame(function () {
      panel.classList.add("whence-open");
      backdrop.classList.add("whence-open");
    });
    panel.focus();
  }

  function waterfall(data) {
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
        var sign = stage.delta < 0 ? MINUS : "+";
        var deltaClass = stage.delta < 0 ? "whence-delta" : "whence-delta whence-delta-added";
        var noun = Math.abs(stage.delta) === 1 ? " row" : " rows";
        dd.appendChild(make("span", { class: deltaClass }, sign + stage.deltaText + noun));
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
        request(id, "rows");
      });
      section.appendChild(button);
    } else if (data.rowsReason) {
      region.appendChild(make("p", { class: "whence-rows-note" }, data.rowsReason));
    }

    section.appendChild(region);
    return section;
  }

  // Put the panel below the value. On a narrow screen the CSS makes it a
  // sheet at the bottom and ignores these positions.
  function place(panel, trigger) {
    if (!trigger) {
      return;
    }
    var rect = trigger.getBoundingClientRect();
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

    var trigger = triggerOf(id);
    if (trigger) {
      trigger.setAttribute("aria-expanded", "false");
      if (restoreFocus) {
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
    var panel = state.panel;
    var region = panel.querySelector(".whence-rows");
    var button = panel.querySelector(".whence-rows-button");
    var restoreFocus = button && document.activeElement === button;
    if (button) {
      button.remove();
    }
    region.removeAttribute("aria-busy");
    region.textContent = "";

    if (!message.ok) {
      var note = make("p", { class: "whence-rows-note", tabindex: "-1" }, message.message);
      region.appendChild(note);
      if (restoreFocus) {
        note.focus();
      }
      return;
    }

    var noun = message.total === 1 ? " row" : " rows";
    var heading = message.totalText + noun;
    if (message.shown < message.total) {
      heading += " (the first " + message.shownText + " are shown)";
    }
    var title = make("h3", { class: "whence-section", tabindex: "-1" }, heading);
    region.appendChild(title);
    if (restoreFocus) {
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

  // --- Registration -----------------------------------------------------

  // --- Clean-up when an output is removed ----------------------------------

  // The panel is in document.body, so it does not go with its output. When
  // Shiny unbinds an output, the panel closes at once. If no output with the
  // same id is bound again soon, the state here is dropped and the server is
  // told, so that it drops the trace and its data.
  function onUnbound(event) {
    var el = event.target;
    if (event.bindingType !== "output" || !el.classList || !el.classList.contains("whence-output")) {
      return;
    }
    var id = el.id;
    var state = outputs[id];
    if (!state) {
      return;
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
    Shiny.outputBindings.register(binding, "whence.explainable");
    Shiny.addCustomMessageHandler("whence:rows", showRows);
    $(document).on("shiny:unbound", onUnbound);
  }

  if (window.Shiny && window.Shiny.OutputBinding) {
    register();
  } else {
    document.addEventListener("DOMContentLoaded", register);
  }
})();

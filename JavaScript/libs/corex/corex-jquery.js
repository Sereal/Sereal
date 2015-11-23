"use strict";
function jQueryHelper() {
    var _svgElements = { altGlyph: 1, altGlyphDef: 1, altGlyphItem: 1, animate: 1, animateColor: 1, animateMotion: 1, animateTransform: 1, circle: 1, clipPath: 1, "color-profile": 1, cursor: 1, defs: 1, desc: 1, ellipse: 1, feBlend: 1, feColorMatrix: 1, feComponentTransfer: 1, feComposite: 1, feConvolveMatrix: 1, feDiffuseLighting: 1, feDisplacementMap: 1, feDistantLight: 1, feFlood: 1, feFuncA: 1, feFuncB: 1, feFuncG: 1, feFuncR: 1, feGaussianBlur: 1, feImage: 1, feMerge: 1, feMergeNode: 1, feMorphology: 1, feOffset: 1, fePointLight: 1, feSpecularLighting: 1, feSpotLight: 1, feTile: 1, feTurbulence: 1, filter: 1, font: 1, "font-face": 1, "font-face-format": 1, "font-face-name": 1, "font-face-src": 1, "font-face-uri": 1, foreignObject: 1, g: 1, glyph: 1, glyphRef: 1, hkern: 1, image: 1, line: 1, linearGradient: 1, marker: 1, mask: 1, metadata: 1, "missing-glyph": 1, mpath: 1, path: 1, pattern: 1, polygon: 1, polyline: 1, radialGradient: 1, rect: 1, script: 1, set: 1, stop: 1, style: 1, svg: 1, "switch": 1, symbol: 1, text: 1, textPath: 1, title: 1, tref: 1, tspan: 1, use: 1, view: 1, vkern: 1, };
    Function.addTo(jQueryHelper, [parseSelector, createElementFromSelectorNode, getOrAppendChildBySelector, createElementFromSelector]);

    function parseSelector(s) {
        var sizzle = jQuery.find;
        var groups = sizzle.tokenize(s);
        return groups;
    }

    function createElementFromSelector(selector) {
        var nodes = parseSelector(selector);
        return createElementFromSelectorNode(nodes[0]);
    }
    function createElementFromSelectorNode(node) {
        var tagName = "div";
        var tagToken = node.first(function (t) { return t.type == "TAG"; });
        if (tagToken != null)
            tagName = tagToken.value;

        var idToken = node.first(function (t) { return t.type == "ID"; });
        var isSvg = _svgElements[tagName];
        var el;
        if (isSvg)
            el = $(document.createElementNS("http://www.w3.org/2000/svg", tagName));
        else
            el = $("<" + tagName + "/>");
        if (idToken != null)
            el.attr("id", idToken.value.substr(1));

        var atts = node.whereEq("type", "ATTR").select(function (t) { return t.value.substr(1, t.value.length - 2).split('='); });
        if (atts.length > 0) {
            atts.forEach(function (att) {
                el.attr(att[0], att[1]);
            });
        }

        var classes = node.whereEq("type", "CLASS").select(function (t) { return t.value.substr(1); });
        if (classes.length > 0) {
            if (isSvg)
                el.attr("class", classes.join(" "));
            else
                el.addClass(classes.join(" "));
        }

        return el;
    }

    function getOrAppendChildBySelector(parentEl, selector, options) {
        var childEls = parentEl.children(selector).toArray();
        var total = null;
        var list = null;
        var action = null;
        var storeDataItem = false;
        var removeRemaining = false;
        var create;
        var destroy;
        if (options != null) {
            if (options.total != null)
                total = options.total;
            if (options.list != null) {
                list = options.list;
                if (total == null)
                    total = list.length;
            }
            action = options.action;
            storeDataItem = options.storeDataItem;
            removeRemaining = options.removeRemaining;
            create = options.create;
            destroy = options.destroy;
        }
        if (total == null)
            total = 1;

        var index = childEls.length;

        if (action != null || storeDataItem) {
            var min = Math.min(index, total);
            if (list == null)
                list = [];
            for (var i = 0; i < min; i++) {
                var child = $(childEls[i]);
                var dataItem = list[i];
                if (storeDataItem)
                    child.dataItem(dataItem);
                if (action != null)
                    action(child, dataItem, index);
            }
        }
        if (index < total) {
            var selectorNodes = parseSelector(selector);
            if (selectorNodes.length != 1)
                throw new Error();
            var selectorNode = selectorNodes[0];
            while (index < total) {
                var dataItem = list != null ? list[index] : null;
                var child = create ? create(dataItem, index) : createElementFromSelectorNode(selectorNode);
                var childEl = child[0];
                parentEl.append(childEl);
                childEls.push(childEl);
                if (storeDataItem)
                    child.dataItem(dataItem);
                if (action != null)
                    action(child, dataItem, index);
                index++;
            }
        }
        if (removeRemaining) {
            while (childEls.length > total) {
                var parentEl = childEls.pop();
                if (destroy)
                    destroy(parentEl);
                $(parentEl).remove();
            }
        }
        return $(childEls);
    }
}
jQueryHelper();

jQuery.fn.getAppend = function (selector, options) {
    return jQueryHelper.getOrAppendChildBySelector(this, selector, options);
}
jQuery.fn.getAppendRemove = function (selector, total) {
    if (typeof (total) == "boolean")
        total = total ? 1 : 0;
    return jQueryHelper.getOrAppendChildBySelector(this, selector, { total: total, removeRemaining: true });
}
jQuery.fn.getAppendRemoveForEach = function (selector, list, action, options) {
    if (options == null)
        options = {};
    options.list = list;
    options.action = action;
    options.removeRemaining = true;
    return jQueryHelper.getOrAppendChildBySelector(this, selector, options);
}
jQuery.create = function (selector) {
    return jQueryHelper.createElementFromSelector(selector);
}

//binds a container's children selector to a list, matching the number of elements to the list.length (creating/deleting elements where needed), optionally performing action(el, dataItem, index) on each element
//returns a new jQuery object containing all children relevant to the selector
jQuery.fn.bindChildrenToList = function (selector, list, action, options) {
    if (options == null)
        options = {};
    options.list = list;
    options.action = action;
    options.storeDataItem = true;
    options.removeRemaining = true;
    return jQueryHelper.getOrAppendChildBySelector(this, selector, options);
}

jQuery.fn.dataItem = function (value) {
    if (arguments.length > 0)
        return this.data("DataItem", value);
    return this.data("DataItem");
}

//Turns a jquery object to an array of single jquery objects
jQuery.fn.toArray$ = function (action) {
    var list = [];
    for (var i = 0; i < this.length; i++)
        list.push($(this[i]));
    return list;
}

//Turns an array of jquery objects to a single jquery object
jQuery.fromArray$ = function (list) {
    return $(list.selectMany(function (j) { return j.toArray(); }));
}

// $.when(promises) with a list of actions instead of dynamic params
jQuery.whenAll = function (list) {
    return jQuery.when.apply(jQuery, list);
}

function jQueryHelper2() {
    var _jQuery_fn_find = jQuery.fn.find;
    var _jQuery_fn_children = jQuery.fn.children;
    Function.addTo(jQuery.fn, [zip, generator, find, forEach, forEach$, children, offOn, adder, remover]);
    var _zippedFunctions = [ofDataItem, existing, added, removed, changed, unchanged];

    function byIndexToByValue(mappings) {
        var mappings2 = mappings.select(function (mapping, i) {
            if (mapping.obj == mapping.prevObj)
                return mapping;
            var existing = mappings.firstEq("prevObj", mapping.obj);
            if (existing == null)
                return mapping;
            else
                return { obj: mapping.obj, el: existing.el, index: mapping.index, prevObj: existing.prevObj };
        });
        return mappings2;
    }
    function adder(func) {
        if(arguments.length==0)
            return this._adder;
        this._adder = func;
        return this;
    }
    function remover(func) {
        if(arguments.length==0)
            return this._remover;
        this._remover = func;
        return this;
    }
    function zip(list1, opts) {
        var list2 = this.toArray();

        var mappings = list1.selectWith(list2, function (obj, el, index) { return { obj: obj, el: el, index: index, prevObj: el == null ? null : $(el).dataItem() }; });
        var byValue = opts != null && opts.joinBy == "value";
        if (byValue)
            mappings = byIndexToByValue(mappings);

        var added = [];
        var removed = [];
        var existing = [];
        var changed = [];
        var unchanged = [];
        var selector = this.originalSelector;
        var prevObject = this.prevObject;
        var generator = this.generator();


        var autoAdd = opts == null || opts.autoAdd || prevObject!=null;
        var autoRemove = opts == null || opts.autoRemove || true;
        var adder = this._adder || function (el) { prevObject.append(el); };
        var remover = this._remover || function (el) { el.remove(); };


        mappings.forEach(function (mapping) {
            if (mapping.el == null) {
                mapping.el = generator(mapping.obj)[0];
                added.push(mapping);
                return;
            }
            if (mapping.obj == null) {
                removed.push(mapping);
                return;
            }
            if (mapping.prevObj != mapping.obj)
                changed.push(mapping);
            else
                unchanged.push(mapping);
            existing.push(mapping);
        });


        changed.forEach(function (mapping) { $(mapping.el).dataItem(mapping.obj); });
        added.forEach(function (mapping) { $(mapping.el).dataItem(mapping.obj); });

        if (autoRemove)
            removed.forEach(function (mapping) { remover(mapping.el); })

        if (autoAdd)
            added.forEach(function (mapping) { adder(mapping.el); })

        var newMappings = existing.concat(added);
        var els = newMappings.select("el");
        
        var q = this.pushStack(els);
        q._zip = { added:added, removed:removed, existing:existing, changed:changed, unchanged:unchanged, mappings:mappings };
        q._originalSelector = this._originalSelector;
        q._generator = this._generator;
        q._adder = this._adder;
        q._remover = this._remover;
        Function.addTo(q, _zippedFunctions);

        return q;
    }

    function ofDataItem(obj) {
        var mapping = _zip.mappings.firstEq("obj", obj);
        if (mapping == null)
            return null;
        return this.pushStack([mapping.el]);
    }

    function existing() {
        return this.pushStack(this._zip.existing.select("el"));
    }
    function added() {
        return this.pushStack(this._zip.added.select("el"));
    }
    function removed() {
        return this.pushStack(this._zip.removed.select("el"));
    }
    function changed() {
        return this.pushStack(this._zip.changed.select("el"));
    }
    function unchanged() {
        return this.pushStack(this._zip.unchanged.select("el"));
    }

    function find(selector) {
        var res = _jQuery_fn_find.apply(this, arguments);
        res._originalSelector = selector;
        return res;
    }
    function children(selector) {
        var res = _jQuery_fn_children.apply(this, arguments);
        res._originalSelector = selector;
        return res;
    }

    function generator(funcOrSelector) {
        if (arguments.length > 0) {
            if (typeof (funcOrSelector) == "string") {
                var selector = funcOrSelector;
                this._generator = function () { return $.create(selector); };
            }
            else {
                this._generator = funcOrSelector;
            }
            return this;
        }
        if (this._generator === undefined) {
            var selector = this._originalSelector;
            if (selector == null)
                throw new Error("Can't resolve selector for this jQuery object", this);
            this._generator = function () { return $.create(selector); };
        }
        return this._generator;
    }

    function forEach(action) {
        this.toArray().forEach(action);
        return this;
    }
    function forEach$(action) {
        this.toArray$().forEach(action);
        return this;
    }
    function offOn(events, handler) {
        return this.off(events).on(events, handler);
    }

};
jQueryHelper2();

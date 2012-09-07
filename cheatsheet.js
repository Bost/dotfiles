var eMaxIdx = 12;
var idPrefix = "head";

function getId(idx) {
    return "#"+idPrefix + idx;
}
function getIds(maxIdx) {
    var ids = "";
    for (var i = 0; i < maxIdx; i++) {
        if (i > 0) {
            ids += ", ";
        }
        ids += getId(i);
    }
    return ids;
}
var ids = getIds(eMaxIdx);

function runEffect(divId) {
    // other effect must be downloaded from jquery theme roller
    var selectedEffect = "blind";

    // most effect types need no options passed by default
    var options = {};
    // some effects have required parameters
    if ( selectedEffect === "scale" ) {
        options = { percent: 0 };
    } else if ( selectedEffect === "size" ) {
        options = { to: { width: 200, height: 60 } };
    }
    //var elem = $( "#"+divId +	" .effect" );
    var elem = $("#"+divId).next();
    elem.toggle( selectedEffect, options, 360 );
};

$(function() {
    $(ids).click(function() {
        runEffect(this.id);
        return false;
    });
});
$(function() {
    var el = $("#sortable");
    //el.sortable();    // this makes copy-paste impossible
    //el.disableSelection();
    //$( ".resizable" ).resizable();
    //$( ".draggable" ).draggable();
});
$(function() {
    $( "input:submit, a, button", ".buttons" ).button();
    $( "a", ".buttons" ).click(function() { return false; });
});
$(function() {
    $("#expand_all").click(function(){
        for (var i = 0; i < eMaxIdx; i++) {
            var accId = getId(i);
            var elem = $(accId).next();
            elem.show();
        }
        return false;
    });

    function collapseAll() {
        for (var i = 0; i < eMaxIdx; i++) {
            var accId = getId(i);
            var elem = $(accId).next();
            elem.hide();
        }
    }
    $("#goto_vim").click(function(){
        collapseAll();
        runEffect("head0");
        // TODO scroll down to the emacs section and open it if closed
        return false;
    });
    $("#goto_emacs").click(function(){
        collapseAll();
        runEffect("head1");
        // TODO scroll down to the emacs section and open it if closed
        return false;
    });
    $("#collapse_all").click(function(){
        collapseAll();
        return false;
    });
});

function addElems(id) {
    var ni = document.getElementById(id);
    if (!ni) {
        return;
    }
    //var numi = document.getElementById('theValue');
    //var num = (document.getElementById('theValue').value -1)+ 2;
    //numi.value = num;
    //var divIdName = 'my'+num+'Div';
    //newdiv.setAttribute('id',divIdName);
    for (var i = 0; i < json.length; i++) {
        cmd = json[i][0];
        desc = json[i][1];
        var newdiv = document.createElement('div');
        newdiv.innerHTML = '<div class="line"><div class="cmd">'+cmd+'</div><div class="desc">'+desc+'</div></div>';
        ni.appendChild(newdiv);
    }
}


$(document).keyup(function(event) {
    if ($("#address").is(":focus") && (event.keyCode == 13)) {
        $("#go").click();
    }
});
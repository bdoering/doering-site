// $(document).ready(function () {
//     $('#interact').hide();
// });

$('#btn-interact').click(function() {
    // $("#interact").toggle('slow');
    if(!$(this).hasClass("active")) {
	$("#interact").removeClass("hidden").addClass("show");
    } else {
	$("#interact").removeClass("show").addClass("hidden");
    };
});

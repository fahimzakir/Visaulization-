$(document).ready(function(){
  $('input[name=select_room]').on('click', function(event){
    if($('input[name=select_room]:checked').length == 0){
      $(this).prop('checked', true);
    }
  });
});
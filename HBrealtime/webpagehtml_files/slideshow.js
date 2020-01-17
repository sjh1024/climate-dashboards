$(document).ready(function() {
    $('#gallery').cycle({
        fx: 'scrollRight',
        timeout: 7000,
        speed: 400,
        delay: 200,
        pager: '#pager',
        next: '#next',
        prev: '#prev'
    });
    $('#playControl').toggle(
        function() {
            $('#gallery').cycle('pause');
            $(this).text('Play');
        },
        function() {
            $('#gallery').cycle('resume');
            $(this).text('Pause');
    });
}); // end ready()

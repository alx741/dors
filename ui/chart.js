var ctx = document.getElementById('chart').getContext('2d');

setInterval(function() {
    fetch('/emotions', {
        method: "GET",
        mode: "cors",
        cache: "no-cache",
        credentials: "same-origin",
        redirect: "follow",
        referrer: "no-referrer",
        body: null,
    })
    .then(res => res.json())
    .then(data => renderData(data))
    .catch(e => reportDataError());
}, 1000);

function reportDataError() {
    alert("Hubo un problema inesperado");
}

function renderData(data) {
    console.log(data);

    let labels = [];
    let points = [];
    for (var i=0; i < data.length; i++) {
        if (data[i].emotion != "None") // Remove neutral emotion
        {
            switch (data[i].emotion)
            {
                case "Anticipation":
                    labels.push("Anticipación");
                    break;

                case "Joy":
                    labels.push("Alegría");
                    break;

                case "Trust":
                    labels.push("Confianza");
                    break;

                case "Fear":
                    labels.push("Miedo");
                    break;

                case "Sadness":
                    labels.push("Tristeza");
                    break;

                case "Disgust":
                    labels.push("Disgusto");
                    break;

                case "Anger":
                    labels.push("Enojo");
                    break;

                case "Surprise":
                    labels.push("Asombro");
                    break;

                case "None":
                    labels.push("Neutral");
                    break;
            }
            points.push(data[i].count);
        }
    }
    renderChart(labels, points);
}

function renderChart(labels, points)
{
    console.log(labels);
    console.log(points);

    var chart = new Chart(ctx, {
        type: 'pie',
        data: {
            labels: labels,
            datasets: [{
                label: 'Circumplejo emocional',
                data: points,
                backgroundColor: [
                    'rgba(255, 99, 132, 1)',
                    'rgba(54, 162, 235, 1)',
                    'rgba(255, 206, 86, 1)',
                    'rgba(75, 192, 192, 1)',
                    'rgba(153, 102, 255, 1)',
                    'rgba(255, 159, 64, 1)',
                    'rgba(255, 99, 132, 1)',
                    'rgba(54, 162, 235, 1)'
                ],
                borderWidth: 5
            }]
        },
    });
}

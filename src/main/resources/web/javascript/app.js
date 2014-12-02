/**
 * Created by alespuh on 26.11.14.
 */
var lotoApp = angular.module('lotoApp', ['highcharts-ng', 'ui.chart']);

lotoApp.controller('GraphicCtrl', function ($scope, $http){

    $scope.pastWindow = 10;
    $scope.futureWindow = 50;

//    $scope.chartDiv2Result = new Array();

    $http.get("/graphicdata1?pW="+$scope.pastWindow+"&&pF="+$scope.futureWindow).success(function(result){
        $scope.chartDiv2Result = result;
        jQuery.jqplot('chartdiv2', [$scope.chartDiv2Result], {
            title: "Топ / повторения по окнам"
            //,series: [{renderer:jQuery.jqplot.BarRenderer}]
        });
    });

    $scope.chartDiv2 = function(){
        $http.get("/graphicdata1?pW="+$scope.pastWindow+"&&pF="+$scope.futureWindow).success(function(result){
            $scope.chartDiv2Result = result;
//            $scope.chartConfig.series.pop();
            $scope.chartConfig.series.push({
                data: result
            });
        });
    };


//    $http.get("/graphicdata1").success(function(result){
//        jQuery.jqplot('chartdiv2', [result], {
//            title: "Топ / повторения по окнам"
//            //,series: [{renderer:jQuery.jqplot.BarRenderer}]
//        });
//    });

    $http.get("/graphicdata2").success(function(result){
        jQuery.jqplot('chartdiv3', [result], {
            title: "топ невыпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах"
            //,series: [{renderer:jQuery.jqplot.BarRenderer}]
        });
    });

    $http.get("/graphicdata3").success(function(result){
        jQuery.jqplot('chartdiv4', [result], {
            title: "топ наименее выпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах"
            //,series: [{renderer:jQuery.jqplot.BarRenderer}]
        });
    });

    $http.get("/graphicdata4").success(function(result){
        jQuery.jqplot('chartdiv5', [result], {
            title: "сколько выпало чисел одного разряда для каждого тиража"
            //,series: [{renderer:jQuery.jqplot.BarRenderer}]
        });
    });

    $http.get("/figureOrderStatistics1").success(function(result){
        jQuery('#figureOrderStatistics1').html(
            "<h3>Выпало разрядов</h3>" +
            "<div>0 - " + result.order0Frequency + "</div>" +
            "<div>1 - " + result.order1Frequency + "</div>" +
            "<div>2 - " + result.order2Frequency + "</div>" +
            "<div>3 - " + result.order3Frequency + "</div>"
        )
    });

    $http.get("/figureOrderStatistics2").success(function(result){
        jQuery('#figureOrderStatistics2').html(
            "<h3>количество выпадений чисел одного разряда</h3>" +
            "<div>2 чисел - " + result.figure2 + "</div>" +
            "<div>3 чисел - " + result.figure3 + "</div>" +
            "<div>4 чисел - " + result.figure4 + "</div>" +
            "<div>5 чисел - " + result.figure5 + "</div>"
        )
    });

    $http.get("/figureDiapasonStatistics").success(function(result){
        jQuery('#figureDiapasonStatistics').html(
            "<h3>количество выпадений диапазонов чисел</h3>" +
            "<div>[0 - 6] - " + result.diapason1 + "</div>" +
            "<div>[7 - 16] - " + result.diapason2 + "</div>" +
            "<div>[17 - 26] - " + result.diapason3 + "</div>" +
            "<div>[27 - 36] - " + result.diapason4 + "</div>"
        )
    });


    $scope.chartOptions = {
        seriesDefaults: {
            // Make this a pie chart.
            renderer: jQuery.jqplot.PieRenderer,
            rendererOptions: {
                // Put data labels on the pie slices.
                // By default, labels show the percentage of the slice.
                showDataLabels: true
            }
        },
        legend: { show:true, location: 'e' }
    };


    /*
        $http.get("/graphicdata").success(function(result){
            $scope.graphicData = result;
            result.forEach(function(elem, index){
                $scope.chartConfig.series.push({name: "Run ".concat(index+1), data: elem})
    //            $scope.chartConfig.series.push({name: "tirage2", data: elem})
    //            $scope.chartConfig.series.push({name: "tirage3", data: elem})
            });
        });

 */
        $scope.chartConfig = {
            options: {
                chart: {
                    type: 'line',
                    width: 2000,
                    height: 500
                }
            },
            xAxis : { categories: []}
            ,
            yAxis : { categories: []}
            ,
            series: [{data: []}]
            ,
            title: {
                text: 'Hello'
            },

            loading: false
        }


});
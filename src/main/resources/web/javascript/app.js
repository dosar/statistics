/**
 * Created by alespuh on 26.11.14.
 */
var lotoApp = angular.module('lotoApp', ["highcharts-ng"]);

lotoApp.controller('GraphicCtrl', function ($scope, $http){

    $http.get("/graphicdata1").success(function(result){
        jQuery.jqplot('chartdiv2', [result], {
            title: "Топ / повторения по окнам"
            //,series: [{renderer:jQuery.jqplot.BarRenderer}]
        });
    });

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

    //$http.get("/graphicdata4").success(function(result){
    //    jQuery.jqplot('chartdiv5', [result], {
    //        title: "сколько выпало чисел одного разряда для каждого тиража"
    //        //,series: [{renderer:jQuery.jqplot.BarRenderer}]
    //    });
    //});
    //
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

    /*
        $http.get("/graphicdata").success(function(result){
            $scope.graphicData = result;
            result.forEach(function(elem, index){
                $scope.chartConfig.series.push({name: "Run ".concat(index+1), data: elem})
    //            $scope.chartConfig.series.push({name: "tirage2", data: elem})
    //            $scope.chartConfig.series.push({name: "tirage3", data: elem})
            });
        });


        $scope.chartConfig = {
            options: {
                chart: {
                    type: 'scatter'
                }
            },
            xAxis : { categories: []}
            ,
            yAxis : { categories: []}
            ,
            series: []
            ,
            title: {
                text: 'Hello'
            },

            loading: false
        }

        */
});
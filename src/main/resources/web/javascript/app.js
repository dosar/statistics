/**
 * Created by alespuh on 26.11.14.
 */
var lotoApp = angular.module('lotoApp', ['highcharts-ng', 'ui.chart']);

lotoApp.controller('GraphicCtrl', function ($scope, $http){


    $scope.windows =
    {
        graphic1: {pastWindow: 10, futureWindow: 10},
        graphic2: {pastWindow: 10, futureWindow: 10},
        graphic3: {pastWindow: 10, futureWindow: 10}
    }


    $scope.chart1 = function(){
        $http.get("/graphicdata1?pW="+$scope.windows.graphic1.pastWindow+"&&pF="+$scope.windows.graphic1.futureWindow).success(function(result){
            if(typeof $scope.chartConfig1.series !== 'undefined' && $scope.chartConfig1.series.length > 0)
            {
                $scope.chartConfig1.series = [];
            }
            $scope.chartConfig1.series.push({
                data: result
            });
        });
    };

    $scope.chart2 = function(){
        $http.get("/graphicdata2?pW="+$scope.windows.graphic2.pastWindow+"&&pF="+$scope.windows.graphic2.futureWindow).success(function(result){
            if(typeof $scope.chartConfig2.series !== 'undefined' && $scope.chartConfig2.series.length > 0)
            {
                $scope.chartConfig2.series = [];
            }
            $scope.chartConfig2.series.push({
                data: result
            });
        });
    };

    $scope.chart3 = function(){
        $http.get("/graphicdata3?pW="+$scope.windows.graphic3.pastWindow+"&&pF="+$scope.windows.graphic3.futureWindow).success(function(result){
            if(typeof $scope.chartConfig3.series !== 'undefined' && $scope.chartConfig3.series.length > 0)
            {
                $scope.chartConfig3.series = [];
            }
            $scope.chartConfig3.series.push({
                data: result
            });
        });
    };


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


    $scope.chartConfig1 = {
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
        series: [{data:  []}]
        ,
        title: {
            text: 'Топ / повторения по окнам'
        },

        loading: false
    }

    $scope.chartConfig2 = {
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
        series: [{data:  []}]
        ,
        title: {
            text: 'Топ невыпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах'
        },

        loading: false
    }

    $scope.chartConfig3 = {
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
        series: [{data:  []}]
        ,
        title: {
            text: 'Топ наименее выпадающих чисел за несколько пред. тиражей и смотрим выпадают ли они в нескольких след. тиражах'
        },

        loading: false
    }

    $scope.chartConfig4 = {
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
        series: [{data:  []}]
        ,
        title: {
            text: 'Сколько выпало чисел одного разряда для каждого тиража'
        },

        loading: false
    }


});
/**
 * Created by alespuh on 26.11.14.
 */
var lotoApp = angular.module('lotoApp', ['highcharts-ng', 'ui.chart', 'ngRoute']);

lotoApp.config(function($routeProvider){
   $routeProvider
       .when('/', {
           templateUrl: 'maingraphics.html',
           controller: 'GraphicCtrl'
       })
       .when('/data', {
           templateUrl: 'runresults.html',
           controller: 'DataCtrl'
       })
       .when('/trustedIntervals', {
           templateUrl: 'trustedintervals.html',
           controller: 'TiCtrl'
       })
       .when('/occurencies', {
           templateUrl: 'occurencies.html',
           controller: 'OccCtrl'
       })
       .when('/detailedata', {
           templateUrl: 'detailed_runresults.html',
           controller: 'DetailedResultsCtrl'
       });
});

lotoApp.controller('OccCtrl', function($scope, $http){

    $http.get("/occurencies").success(function(result){
        $scope.chartConfig.series = [];
        $scope.chartConfig.series.push({data: result});
    });

    $scope.chartConfig = {
        options: {
            chart: {
                type: 'line'
                //width: 2000,
                //height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Частота выпадения чисел'
        },
        loading: false
    }
});

lotoApp.controller('TiCtrl', function($scope, $http){

    $scope.window = 10;
    $scope.p1 = 90;
    $scope.p2 = 90;
    $scope.p3 = 90;
    $scope.p4 = 90;
    $scope.p5 = 90;

    $scope.getData = function(){
        $http.get("/trustedintervals?pw=" + $scope.window + "&&p1=" + $scope.p1 + "&&p2=" + $scope.p2 + "&&p3=" + $scope.p3 +
            "&&p4=" + $scope.p4 + "&&p5=" + $scope.p5).success(function(result){
                $scope.chartConfig.series = [];
                $scope.pushInterval($scope.chartConfig, result, 0, "blue");
                $scope.pushInterval($scope.chartConfig, result, 1, "green");
                $scope.pushInterval($scope.chartConfig, result, 2, "yellow");
                $scope.pushInterval($scope.chartConfig, result, 3, "red");
                $scope.pushInterval($scope.chartConfig, result, 4, "black");
            });
    };

    $scope.pushInterval = function(config, result, index, color) {
        config.series.push({
            data: jQuery.map(result, function (elem) {
                return elem[index][0];
            }),
            color: color
        });
        config.series.push({
            data: jQuery.map(result, function (elem) {
                return elem[index][1];
            }),
            color: color
        });
    };

    $scope.chartConfig = {
        options: {
            chart: {
                type: 'line'
                //width: 2000,
                //height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Доверительные интервалы'
        },
        loading: false
    }
});

lotoApp.controller('MainCtrl', function($scope){
   $scope.message;
});

lotoApp.controller('DataCtrl', function ($scope, $http) {

    $scope.renderOneFigure = function (figure, intersected){
        var additionalClass = intersected ? " badge" : "";
        return '<div class="col-xs-6' + additionalClass + '">' + figure + '</div>';
    };

    $http.get("/runresults").success(function(result){
        var divs = jQuery.map(result, function(elem){
            return '<tr>' +
                '<td>' + $scope.renderOneFigure(elem[0][0], elem[0][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[1][0], elem[1][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[2][0], elem[2][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[3][0], elem[3][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[4][0], elem[4][1]) + '</td>' +
            '</tr>';
        });
        jQuery("#rrs").append(divs);
    });
});


lotoApp.controller('DetailedResultsCtrl', function ($scope, $http) {
    $scope.items = [];
    $scope.pastWindow = 10;
    $scope.skipWindow = 0;
    $scope.futureWindow = 10;
    $scope.startFigure = 1;
    $scope.endFigure = 36;
    $scope.topFiguresCount = 7;
    $scope.customBets = [[]];

    $scope.highlightCustomBet = function(index){
        var results = $scope.items[index].runResults;
        for(var i = 0; i< results.length; i++)
        {
            for(var j =0; j< results[i].result.length; j++)
            {
                var element = results[i].result[j];
                console.log(element.figure);
            }
        }
/*        $scope.items[index].runResults.each(function(runResult){
            runResult.each(function(element){
                console.log(element.figure)
            });
        });*/
    }

    $scope.getData = function(){
        $http.get("/strategydebug?pW=" + $scope.pastWindow + "&&sW=" +
        $scope.skipWindow + "&&fW="+$scope.futureWindow +
        "&&tFC=" + $scope.topFiguresCount + "&&sF=" + $scope.startFigure +
        "&&eF=" + $scope.endFigure).success(function(result){
            $scope.items = result;
            for(var i = 0; i< result.length; i++){
                $scope.customBets[i] = [];
            }
        });
    }

    $scope.renderOneFigure = function (figure, intersected){
        var additionalClass = intersected ? " badge" : "";
        return '<div class="col-xs-6' + additionalClass + '">' + figure + '</div>';
    };

    $http.get("/runresults").success(function(result){
        var divs = jQuery.map(result, function(elem){
            return '<tr>' +
                '<td>' + $scope.renderOneFigure(elem[0][0], elem[0][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[1][0], elem[1][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[2][0], elem[2][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[3][0], elem[3][1]) + '</td>' +
                '<td>' + $scope.renderOneFigure(elem[4][0], elem[4][1]) + '</td>' +
                '</tr>';
        });
        jQuery("#rrs").append(divs);
    });
});



lotoApp.controller('GraphicCtrl', function ($scope, $http){

    $scope.windows =
    {
        graphic1: {pastWindow: 10, futureWindow: 10},
        graphic2: {pastWindow: 10, futureWindow: 10},
        graphic3: {pastWindow: 10, futureWindow: 10},
        graphic4: {pastWindow: 10, skipWindow: 0, futureWindow: 10},
        graphic5: {pastWindow: 10, skipWindow: 0, futureWindow: 10},
        graphic6: {pastWindow: 10, skipWindow: 0, futureWindow: 10, topFiguresCount: 7, startFigure: 16, endFigure: 36},
        graphic7: {pastWindow: 10}
    }

    $scope.chart1 = function(){
        $http.get("/graphicdata1?pW="+$scope.windows.graphic1.pastWindow+"&&pF="+$scope.windows.graphic1.futureWindow).success(function(result){
            $scope.chartConfig1.series = [];
            $scope.chartConfig1.series.push({
                data: result
            });
        });
    };

    $scope.chart2 = function(){
        $http.get("/graphicdata2?pW="+$scope.windows.graphic2.pastWindow+"&&pF="+$scope.windows.graphic2.futureWindow).success(function(result){
            $scope.chartConfig2.series = [];
            $scope.chartConfig2.series.push({
                data: result
            });
        });
    };

    $scope.chart3 = function(){
        $http.get("/graphicdata3?pW="+$scope.windows.graphic3.pastWindow+"&&pF="+$scope.windows.graphic3.futureWindow).success(function(result){
            $scope.chartConfig3.series = [];
            $scope.chartConfig3.series.push({
                data: result
            });
        });
    };

    $scope.chart4 = function(){
        $http.get("/graphicdata4?pW="+$scope.windows.graphic4.pastWindow+"&&sF="+$scope.windows.graphic4.skipWindow+"&&pF="+$scope.windows.graphic4.futureWindow).success(function(result){
            $scope.chartConfig4.series = [];
            $scope.chartConfig4.series.push({
                data: jQuery.map(result, function(elem){
                    return elem[0];
                })
            });
            $scope.chartConfig4.series.push({
                data: jQuery.map(result, function(elem){
                    return elem[1];
                })
            });
        });
    };


    $scope.chart5 = function(){
        $http.get("/graphicdata5?pW="+$scope.windows.graphic5.pastWindow+"&&sF="+$scope.windows.graphic5.skipWindow+"&&pF="+$scope.windows.graphic5.futureWindow).success(function(result){
            $scope.chartConfig5.series = [];
            $scope.chartConfig5.series.push({
                data: jQuery.map(result, function(elem){
                    return elem[0];
                })
            });
            $scope.chartConfig5.series.push({
                data: jQuery.map(result, function(elem){
                    return elem[1];
                })
            });
        });
    };

    $scope.chart6 = function(){
        $http.get("/graphicdata6?pW=" + $scope.windows.graphic6.pastWindow + "&&sW=" +
            $scope.windows.graphic6.skipWindow + "&&fW="+$scope.windows.graphic6.futureWindow +
            "&&tFC=" + $scope.windows.graphic6.topFiguresCount + "&&sF=" + $scope.windows.graphic6.startFigure +
            "&&eF=" + $scope.windows.graphic6.endFigure).success(function(result){
                $scope.chartConfig6.series = [];
                $scope.chartConfig6.series.push({
                    data: jQuery.map(result, function(elem){
                        return elem[0];
                    })
                });
                $scope.chartConfig6.series.push({
                    data: jQuery.map(result, function(elem){
                        return elem[1];
                    })
                });
        });
    };

    $http.get("/graphicdata0").success(function(result){
        $scope.chartConfig0.series = [];
        $scope.chartConfig0.series.push({
            data: jQuery.map(result, function(elem){
                return elem[0];
            })
        });
        $scope.chartConfig0.series.push({
            data: jQuery.map(result, function(elem){
                return elem[1];
            })
        });
        $scope.chartConfig0.series.push({
            data: jQuery.map(result, function(elem){
                return elem[2];
            })
        });
        $scope.chartConfig0.series.push({
            data: jQuery.map(result, function(elem){
                return elem[3];
            })
        });
        $scope.chartConfig0.series.push({
            data: jQuery.map(result, function(elem){
                return elem[4];
            })
        });
    });

    $scope.pushInterval = function(config, result, index, color) {
        config.series.push({
            data: jQuery.map(result, function (elem) {
                return elem[0][index];
            }),
            color: color
        });
        config.series.push({
            data: jQuery.map(result, function (elem) {
                return elem[1][index];
            }),
            color: color
        });
    };

    $scope.chart7 = function() {
        $http.get("/graphicdata7?pW=" + $scope.windows.graphic7.pastWindow).success(function (result) {
            $scope.chartConfig7.series = [];
            $scope.pushInterval($scope.chartConfig7, result, 0, "blue");
            $scope.pushInterval($scope.chartConfig7, result, 1, "green");
            $scope.pushInterval($scope.chartConfig7, result, 2, "yellow");
            $scope.pushInterval($scope.chartConfig7, result, 3, "red");
            $scope.pushInterval($scope.chartConfig7, result, 4, "black");
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

    $http.get("/figureIntersectionStatistics").success(function(result){
        jQuery('#figureIntersectionStatistics').html(
            "<h3>Пересечений соседних тиражей</h3>" +
            "<div>" + result[0][0] + " - " + result[0][1] + "</div>" +
            "<div>" + result[1][0] + " - " + result[1][1] + "</div>" +
            "<div>" + result[2][0] + " - " + result[2][1] + "</div>" +
            "<div>" + result[3][0] + " - " + result[3][1] + "</div>" +
            "<div>" + result[4][0] + " - " + result[4][1] + "</div>"
        )
    });

    $scope.chartConfig1 = {
        options: {
            chart: {
                type: 'line'
                //width: 2000,
                //height: 500
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
                type: 'line'
                //width: 2000,
                //height: 500
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
                type: 'line'
                //width: 2000,
                //height: 500
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
                type: 'line'
                //width: 2000,
                //height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Стратегия 1'
        },

        loading: false
    }

    $scope.chartConfig5 = {
        options: {
            chart: {
                type: 'line'
                //width: 2000,
                //height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Стратегия 1 без непопулярных чисел'
        },

        loading: false
    }

    $scope.chartConfig6 = {
        options: {
            chart: {
                type: 'line'
                //width: 2000,
                //height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Стратегия 3 без непопулярных чисел'
        },

        loading: false
    }

    $scope.chartConfig0 = {
        options: {
            chart: {
                type: 'line'
                ,width: 8000
                ,height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Просто результаты тиражей'
        },

        loading: false
    }

    $scope.chartConfig7 = {
        options: {
            chart: {
                type: 'line'
                ,width: 2000
                ,height: 500
            }
        },
        xAxis : { categories: []}
        ,
        yAxis : { categories: []}
        ,
        series: [{data:  []}]
        ,
        title: {
            text: 'Интервалы по окну'
        },

        loading: false
    }

});


/**
 * Created by alespuh on 26.11.14.
 */
var lotoApp = angular.module('lotoApp',["highcharts-ng"]);

lotoApp.controller('GraphicCtrl', function ($scope, $http){

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
});
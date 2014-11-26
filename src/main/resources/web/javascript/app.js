/**
 * Created by alespuh on 26.11.14.
 */
var lotoApp = angular.module('lotoApp', [$http]);
lotoApp.controller('GraphicCtrl', function ($scope){
    var data = $http.get("/graphicdata")
});
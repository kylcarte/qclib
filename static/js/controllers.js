'use strict';

var libraryControllers = angular.module('libraryControllers', []);

libraryControllers.controller('PatronCtrl', ['$scope', '$http', 'Patron',
    function($scope, $http, Patron) {

        $scope.patrons = Patron.query();

        $scope.addPatron = function() {
            var newPatron =
                { number: $scope.patronNumber
                };
            Patron.save(newPatron, function (patron) {
                $scope.patrons.push(patron);
            });
            $scope.patronNumber = '';
        };

        $scope.deletePatron = function(pid) {
            var newid = { id: pid };
            $http({method:'DELETE', url: ('/api/patron/' + pid)}).
                success(function() {
                    var newpatrons = $scope.patrons.filter(
                          function(p) { return (p.id != pid); });
                    $scope.patrons = newpatrons;
                }).
                error(function() { });
            // Patron.remove(newid, function (ps) {
            // });
        };

    }]);


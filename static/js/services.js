'use strict';

var libraryServices = angular.module('libraryServices', ['ngResource']);

libraryServices.factory('Patron', ['$resource',
    function($resource) {
        return $resource('/api/patron', {}, {
            query  : { method: 'GET', isArray:true },
            save   : { method: 'POST' },
        });
    }]);


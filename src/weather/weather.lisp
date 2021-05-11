(ql:quickload :rest-helpers)

(use-package :rest-helpers)

(defvar *weather-endpoint* "http://api.openweathermap.org/data/2.5/weather")

(defvar *api-key* (uiop:getenv "OPEN_WEATHER_MAP_API_KEY"))

(defun make-request-url (query)
    (concatenate 'string *weather-endpoint* "?q=" query "&APPID=" *api-key*))

(defun get-weather (query)
    (get-json (make-request-url query)))

(print (get-weather "Singapore"))

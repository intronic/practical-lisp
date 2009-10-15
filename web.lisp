(in-package :com.gigamonkeys.web)

(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(let* ((limit-string (or (request-query-value "limit" request) ""))
	       (limit (or (parse-integer limit-string :junk-allowed t) 1000)))
	  (html
	    (:html
	      (:head (:title "Random"))
	      (:body
	       (:p "Random number: " (:print (random limit)))))))))))

(define-html-macro :standard-page ((&key title) &body body)
  `(:html
    (:head (:title ,title))
    (:body 
     (:h1 ,title)
     ,@body)))

(defun show-query-params (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	 (:standard-page
	  (:title "Query Parameters")
	  (if (request-query request)
	      (html 
	       (:table :border 1
		       (loop for (k . v) in (request-query request)
			    do (html (:tr (:td k) (:td v))))))
	      (html (:p "No query parameters.")))))))))

(defun simple-form (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html 
	  (:html 
	    (:head (:title "Simple Form"))
	    (:body
	     (:form :method "POST" :action "/show-query-params"
		    (:table
		     (:tr (:td "Foo")
			  (:td (:input :name "foo" :size 20)))
		     (:tr (:td "Password")
			  (:td (:input :name "password" :type "password" :size 20))))
		    (:p (:input :name "submit" :type "submit" :value "Okay")
			(:input :type "reset" :value "Reset"))))))))))

(defun show-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	  (:standard-page
	   (:title "Cookies")
	   (if (null (get-cookie-values request))
	       (html (:p "No cookies."))
	       (html
		 (:table
		  (loop for (key . value) in (get-cookie-values request)
		       do (html (:tr (:td key) (:td value)))))))))))))

(defun set-cookie (request entity)
  (with-http-response (request entity :content-type "text/html")
    (set-cookie-header request :name "MyCookie" :value "a cookie value")
    (with-http-body (request entity)
      (with-html-output ((request-reply-stream request))
	(html
	  (:standard-page
	   (:title "Set Cookie")
	   (:p "Cookie set.")
	   (:p (:a :href "/show-cookies" "Look at cookie jar."))))))))

(defmacro define-url-function (name (request &rest params) &body body)
  (with-gensyms (entity)
    (let ((params (mapcar #'normalize-param params)))
      `(progn
	 (defun ,name (,request ,entity)
	   (with-http-response (,request ,entity :content-type "text/html")
	     (let* (,@(param-bindings name request params))
	       ,@(set-cookies-code name request params)
	       (with-http-body (,request ,entity)
		 (with-html-output ((request-reply-stream ,request))
		   (html ,@body))))))
	 (publish :path ,(format nil "/~(~a~)" name) :function ',name)))))

(defun normalize-param (param)
  (etypecase param
    (list param)
    (symbol `(,param string nil nil))))

(defun param-bindings (function-name request params)
  (loop for param in params
       collect (param-binding function-name request param)))

(defun param-binding (function-name request param)
  (destructuring-bind (name type &optional default sticky) param
    (let ((query-name (symbol->query-name name))
	  (cookie-name (symbol->cookie-name function-name name sticky)))
      `(,name (or
	       (string->type ',type (request-query-value ,query-name ,request))
	       ,@(if cookie-name
		     (list `(string->type ',type
					  (get-cookie-value ,request ,cookie-name))))
	       ,default)))))

(defgeneric string->type (type value))

(defmethod string->type ((type (eql 'string)) value)
  (and (plusp (length value)) value))

(defmethod string->type ((type (eql 'integer)) value)
  (parse-integer (or value "") :junk-allowed t))

(defun get-cookie-value (request name)
  (cdr (assoc name (get-cookie-values request) :test #'string=)))

(defun symbol->query-name (sym)
  (string-downcase sym))

(defun symbol->cookie-name (function-name sym sticky)
  (let ((package-name (package-name (symbol-package function-name))))
    (when sticky
      (ecase sticky
	(:global (string-downcase sym))
	(:package (format nil "~(~a:~a~)" package-name sym))
	(:local (format nil "~(~a:~a:~a~)" package-name function-name sym))))))

(defun set-cookies-code (function-name request params)
  (loop for param in params
       when (set-cookie-code function-name request param) collect it))

(defun set-cookie-code (function-name request param)
  (destructuring-bind (name type &optional default stickie) param
    (declare (ignore type default))
    (if stickie
	`(when ,name
	   (set-cookie-header
	    ,request
	    :name ,(symbol->cookie-name function-name name stickie)
	    :value (princ-to-string ,name))))))
	    

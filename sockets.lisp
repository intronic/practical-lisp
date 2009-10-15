(defgeneric socket-make-stream (socket &key input output
                                       element-type external-format
                                       buffering
                                       timeout
                                       auto-close)
  (:documentation "Find or create a STREAM that can be used for IO on
SOCKET \(which must be connected\).  Specify whether the stream is for
INPUT, OUTPUT, or both \(it is an error to specify neither\).  ELEMENT-TYPE
and EXTERNAL-FORMAT are as per OPEN.  TIMEOUT specifies a read timeout
for the stream."))



(defmethod socket-make-stream ((socket socket)
			       &key input output
			       (element-type 'character)
			       (buffering :full)
			       (external-format :default)
			       timeout
			       (auto-close t))
  "Default method for SOCKET objects. An ELEMENT-TYPE of :DEFAULT
will construct a bivalent stream. Acceptable values for BUFFERING
are :FULL, :LINE and :NONE. Streams will have no TIMEOUT
by default.
The stream for SOCKET will be cached, and a second invocation of this
method will return the same stream. This may lead to oddities if this
function is invoked with inconsistent arguments \(e.g., one might request
an input stream and get an output stream in response\)."
  (let ((stream
	 (and (slot-boundp socket 'stream) (slot-value socket 'stream))))
    (unless stream
      (setf stream (sb-sys:make-fd-stream
		    (socket-file-descriptor socket)
		    :name "a socket"
		    :dual-channel-p t
		    :input input
		    :output output
		    :element-type element-type
		    :buffering buffering
		    :external-format external-format
		    :timeout timeout
		    :auto-close auto-close)))
    (setf (slot-value socket 'stream) stream)
    (sb-ext:cancel-finalization socket)
    stream))

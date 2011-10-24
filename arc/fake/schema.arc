(deftem user
  login nil
  password nil
  role nil)

(deftem address
  line1 nil
  line2 nil
  city nil
  state nil
  postal-code nil)

(deftem organization
  name nil
  address nil
  phone nil
  fax nil
  email nil
  contact nil
  url nil
  created-at nil)

(deftem event
  organization-ref nil
  url nil
  start-on nil
  end-on nil
  registration-start-on nil
  registration-end-on nil
  min-age nil
  max-age nil
  price nil
  title nil
  description nil
  tags nil
  procedures nil
  created-at nil)

(deftem comment
  event-ref nil
  rating nil
  comment nil
  approved-p nil
  created-at nil)

(deftem submission
  submitter-email nil
  submission nil
  created-at nil)

(deftem email-reminder
  email nil
  event-ref nil
  days-before-registration nil
  days-before-event nil
  registration-sent-on nil
  event-sent-on nil)

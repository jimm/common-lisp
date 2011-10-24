#include <CoreMIDI/MIDIServices.h>

#define CFSTRING_BUF_SIZE 512

/*
typedef struct {
  unsigned long receiver;
  unsigned long symbol;
} RubyCallback;
*/

/* Caller must release the CFStringRef that is created */
#define CSTR2CFREF(cstr) CFStringCreateWithCString(NULL, cstr, 0)

static MIDIClientRef default_client;

unsigned long default_client_value()
{
  return (unsigned long)default_client;
}

/* Puts string property value into buf. Assumes sizeof(buf) == CFSTRING_BUF_SIZE. */
void midi_obj_string_property(MIDIObjectRef obj, CFStringRef property_name, char *buf)
{
  CFStringRef pvalue;
  MIDIObjectGetStringProperty(obj, property_name, &pvalue);
  CFStringGetCString(pvalue, buf, CFSTRING_BUF_SIZE, 0);
  CFRelease(pvalue);
}

// WARNING: returns address of string on the stack
char * property(unsigned long obj_ref, char *prop_name)
{
  char value[CFSTRING_BUF_SIZE];
  CFStringRef cf_prop_name = CSTR2CFREF(prop_name);
  midi_obj_string_property((MIDIObjectRef)obj_ref, cf_prop_name, value);
  CFRelease(cf_prop_name);
  return value;
}

unsigned long num_sources()
{
  return MIDIGetNumberOfSources();
}

unsigned long num_destinations()
{
  return MIDIGetNumberOfDestinations();
}

/*
VALUE sources(VALUE self)
{
  VALUE arr = rb_ary_new();
  ItemCount nendpoints, i;

  nendpoints = MIDIGetNumberOfSources();
  for (i = 0; i < nendpoints; ++i)
    rb_ary_push(arr, INT2NUM((unsigned long)MIDIGetSource(i)));
  return arr;
}

VALUE destinations(VALUE self)
{
  VALUE arr = rb_ary_new();
  ItemCount nendpoints, i;

  nendpoints = MIDIGetNumberOfDestinations();
  for (i = 0; i < nendpoints; ++i)
    rb_ary_push(arr, INT2NUM((unsigned long)MIDIGetDestination(i)));
  return arr;
}

VALUE create_output_port(VALUE self, VALUE port_name)
{
  MIDIPortRef port = NULL;
  CFStringRef pname = CSTR2CFREF(port_name);
  MIDIOutputPortCreate(default_client, pname, &port);
  CFRelease(pname);
  return INT2NUM((unsigned long)port);
}

VALUE byte_array_to_ruby_array(Byte data[256])
{
  VALUE byte_array = rb_ary_new();
  int i;
  for (i = 0; i < 256; ++i)
    rb_ary_push(byte_array, data[i]);
  return byte_array;
}

void midi_read_func(const MIDIPacketList *pktlist, void *refCon, void *connRefCon)
{
  MIDIPacket *packet = (MIDIPacket *)pktlist->packet;    // remove const
  RubyCallback *rcb = (RubyCallback *)refCon;
  unsigned int i;
  for (i = 0; i < pktlist->numPackets; ++i) {
    int j;
    for (j = 0; j < packet->length; ++j) {
      rb_funcall(rcb->receiver, rcb->symbol, 3, INT2NUM(packet->timeStamp),
		 INT2FIX(packet->length), byte_array_to_ruby_array(packet->data));
    }
    packet = MIDIPacketNext(packet);
  }
}

VALUE create_input_port(VALUE self, VALUE port_name, VALUE read_func_symbol)
{
  MIDIPortRef port = NULL;
  CFStringRef pname = CSTR2CFREF(port_name);

  RubyCallback *rcb = malloc(sizeof(RubyCallback));
  rcb->receiver = self;
  rcb->symbol = rb_to_id(read_func_symbol);

  MIDIInputPortCreate(default_client, pname, midi_read_func, rcb, &port);
  CFRelease(pname);
  return INT2NUM((unsigned long)port);
}

VALUE connect_source(VALUE self, VALUE rport, VALUE rsource)
{
  MIDIPortRef port = (MIDIPortRef)NUM2INT(rport);
  MIDIEndpointRef source = (MIDIEndpointRef)NUM2INT(rsource);
  MIDIPortConnectSource(port, source, NULL);
  return self;
}

VALUE disconnect_source(VALUE self, VALUE rport, VALUE rsource)
{
  MIDIPortRef port = (MIDIPortRef)NUM2INT(rport);
  MIDIEndpointRef source = (MIDIEndpointRef)NUM2INT(rsource);
  MIDIPortDisconnectSource(port, source);
  return self;
}

VALUE send(VALUE self, VALUE rport, VALUE rendpoint, VALUE rtime, VALUE packed_data)
{
  MIDIPortRef port = (MIDIPortRef)NUM2INT(rport);
  MIDIEndpointRef endpoint = (MIDIEndpointRef)NUM2INT(rendpoint);
  MIDITimeStamp time = (MIDITimeStamp)NUM2INT(rtime);

  MIDIPacketList plist;
  MIDIPacket *packet = MIDIPacketListInit(&plist);
  // Returns next packet. If it is 0, the add failed.
  MIDIPacketListAdd(&plist, sizeof(plist), packet, time, RSTRING(packed_data)->len, (Byte *)RSTRING(packed_data)->ptr);
  MIDISend(port, endpoint, &plist);

  return self;
}
*/

// ****************************************************************

void init_osx_midi()
{
  MIDIClientCreate(CFSTR("OsxMidi"), NULL, NULL, &default_client);
}

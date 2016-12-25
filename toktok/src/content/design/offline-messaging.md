# Offline messaging

This is a proposal for adding offline messages to Tox protocol.

# Objective

Currently both contacts have to be online to send/receive messages. This leads
to bad user experience compared to other centralized messengers.

Some clients bypass this issue by using faux offline messaging - queuing message
on client and sending it to given contact as soon as she appears online. However
this approach requires client to be up and running all the time. This is a big
issue for mobile clients - it is not possible to run app all the time (iOS) or
it consumes too much battery/traffic (Android).

Real offline messaging, proposed in this document, should solve issues described
above.

# Background

*Related work in the TokTok or Tox projects? What similar projects exist? Links
to external documents/wikipedia? Nothing about design/requirements here, just
background.*

TODO
- github discussion & requirements from there
  - https://github.com/irungentoo/toxcore/issues/186
  - https://github.com/irungentoo/toxcore/issues/870
  - https://github.com/irungentoo/toxcore/issues/1432
- bittorrent offline storage http://libtorrent.org/dht_store.html ___
- freenet offline storage? https://github.com/TokTok/c-toxcore/issues/86
- check OTR https://github.com/irungentoo/toxcore/issues/1432
- check axolotl protocol ^
- check TextSecure async messages https://whispersystems.org/blog/asynchronous-security/#the-textsecure-protocol
- check chord
  - this guy should use it https://github.com/diasdavid/webrtc-explorer
  - https://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf __
      Time-Shared Storage for nodes with intermittent connectivity. If a person
      wishes some data to be always available, but their machine is only
      occasionally available, they can offer to store others’ data while they
      are up, in return for having their data stored elsewhere when they are
      down.
- check toxmail? https://github.com/toxmail/toxmail/issues/1 - federated - no
- check bitmessage
- other examples?

# Requirements

*Who are the customers for the solution? What are their needs? What is the
problem space? This section estimates scale requirements. How much data needs
to be stored/processed? What kind of data? What about latency/throughput/etc
network requirements? How about growth?*

- perfect forward security
- deniability
- privacy (no one should know who is talking with whom)

# Design Ideas

*Overview of the design. If you have multiple viable ideas, list them all with
pros and cons. Do not include code, only type signatures and explanations. Use
diagrams if necessary. Major structural elements go here. Which existing
technologies will be used? Which components will you write? How do they
integrate? How will others integrate with them? What scaling parameters need to
be considered most prominently? How will the product be rolled out to users?
Implementation strategies go here, implementation does not.*

# Alternatives Considered

*Describe alternatives to the design. Where choices within the design were made,
write those choices inline in the document. This section contains completely
different approaches, their pros and cons, and the reason for not choosing
them. Also include “doing nothing” as one alternative.*

- broadcast to everyone (bitmessage does that?) - does not scale.
- federated servers - more centralization, need people to run those, can be captured
  by Macrohard (with providing one server).
- store message in common contact - should be online, can have 0 common friends

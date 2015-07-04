cmake_minimum_required(VERSION 2.6)
project(libtorrent)
set (SOVERSION "8")
set (VERSION "1.0.5")

set(sources
    web_connection_base
    alert
    alert_manager
    allocator
    asio
    assert
    bandwidth_limit
    bandwidth_manager
    bandwidth_queue_entry
    bloom_filter
    chained_buffer
    connection_queue
    create_torrent
    disk_buffer_holder
    entry
    error_code
    file_storage
    lazy_bdecode
    escape_string
    string_util
    file
    gzip
    hasher
    http_connection
    http_stream
    http_parser
    i2p_stream
    identify_client
    ip_filter
    ip_voter
    peer_connection
    bt_peer_connection
    web_peer_connection
    http_seed_connection
    instantiate_connection
    natpmp
    packet_buffer
    piece_picker
    policy
    puff
    random
    rss
    session
    session_impl
    settings
    socket_io
    socket_type  
    socks5_stream
    stat
    storage
    time
    timestamp_history
    torrent
    torrent_handle
    torrent_info
    tracker_manager
    http_tracker_connection
    utf8
    udp_tracker_connection
    udp_socket
    upnp
    utp_socket_manager
    utp_stream
    logger
    file_pool
    lsd
    disk_buffer_pool
    disk_io_thread
    enum_net
    broadcast_socket
    magnet_uri
    parse_url
    ConvertUTF
    thread
    xml_parse

# -- extensions --
    metadata_transfer
    ut_pex
    ut_metadata
    smart_ban
    lt_trackers
)

# -- kademlia --
set(kademlia_sources
    dht_tracker
    node
    refresh
    rpc_manager
    find_data
    node_id
    routing_table
    traversal_algorithm
    logging
    item
    get_peers
    get_item
)

# -- ed25519 --
set(ed25519_sources
    add_scalar
    fe
    ge
    key_exchange
    keypair
    sc
    seed
    sha512
    sign
    verify
)

set(includes include ed25519/src)

option(LIBTORRENT_pool-allocators "Uses a pool allocator for disk and piece buffers" ON)
option(LIBTORRENT_dht "enable support for Mainline DHT" ON)
option(LIBTORRENT_unicode "enable unicode support" ON)

set(CMAKE_CONFIGURATION_TYPES Debug Release RelWithDebInfo)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release FORCE)
endif ()

# add_definitions() doesn't seem to let you say wich build type to apply it to
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DTORRENT_DEBUG")
if (UNIX)
    set(CMAKE_C_FLAGS_RELWITHDEBINFO "-Os -g")
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO}")
endif ()

list(APPEND sources sha1)

foreach(s ${sources})
    list(APPEND sources2 src/${s})
endforeach ()

if (LIBTORRENT_dht)
    foreach(s ${kademlia_sources})
        list(APPEND sources2 src/kademlia/${s})
    endforeach(s)
    foreach(s ${ed25519_sources})
        list(APPEND sources2 ed25519/src/${s})
    endforeach(s)
endif ()

add_library(libtorrent STATIC ${sources2})

target_include_directories(libtorrent PUBLIC ${includes})
target_compile_definitions(libtorrent PUBLIC
    TORRENT_DISABLE_ENCRYPTION
    TORRENT_DISABLE_RESOLVE_COUNTRIES
    TORRENT_DISABLE_GEO_IP
    BOOST_ASIO_SEPARATE_COMPILATION
    BOOST_EXCEPTION_DISABLE
    BOOST_ASIO_ENABLE_CANCELIO
    _FILE_OFFSET_BITS=64
)

if (NOT LIBTORRENT_dht)
    target_compile_definitions(libtorrent PUBLIC TORRENT_DISABLE_DHT)
endif ()

if (NOT LIBTORRENT_pool-allocators)
    target_compile_definitions(libtorrent PUBLIC TORRENT_DISABLE_POOL_ALLOCATOR)
endif()

if (LIBTORRENT_unicode)
    add_definitions(-DUNICODE -D_UNICODE)
endif()


if (NOT MSVC)
    target_compile_options(libtorrent PUBLIC "-fvisibility=hidden" "-fvisibility-inlines-hidden")
    # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fvisibility=hidden")
    # set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -fvisibility-inlines-hidden")
endif ()

# Boost
set(Boost_USE_STATIC_LIBS ON)
if (NOT DEFINED Boost_INCLUDE_DIR OR NOT DEFINED Boost_LIBRARIES)
    find_package(Boost COMPONENTS system thread date_time chrono)
endif ()
target_include_directories(libtorrent PUBLIC ${Boost_INCLUDE_DIR})
target_link_libraries(libtorrent ${Boost_LIBRARIES})

if (WIN32)
    target_link_libraries(libtorrent wsock32 ws2_32 Iphlpapi.lib)
    target_compile_definitions(libtorrent PUBLIC "_WIN32_WINNT=0x0600")
endif ()

# this works around a bug in asio in boost-1.39
#add_definitions(-DBOOST_ASIO_HASH_MAP_BUCKETS=1021  -D__USE_W32_SOCKETS -DWIN32_LEAN_AND_MEAN )

if (MSVC)
    target_compile_options(libtorrent PUBLIC "/EHsc" "/Zc:wchar_t" "/Zc:forScope" "/MP")
    target_compile_definitions(libtorrent PUBLIC _SCL_SECURE_NO_DEPRECATE _CRT_SECURE_NO_DEPRECATE)
else ()
    target_compile_options(libtorrent PUBLIC "-fexceptions" "-Wno-c++11-extensions")
endif ()

if (${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang")
    target_compile_options(libtorrent PUBLIC "-fcolor-diagnostics")
endif ()

set_target_properties(libtorrent PROPERTIES
    SOVERSION ${SOVERSION})

get_property(COMPILETIME_OPTIONS_LIST
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIRECTORY}
    PROPERTY COMPILE_DEFINITIONS
    )
foreach (s ${COMPILETIME_OPTIONS_LIST})
    set (COMPILETIME_OPTIONS "${COMPILETIME_OPTIONS} -D${s}")
endforeach ()

configure_file(libtorrent-rasterbar-cmake.pc.in libtorrent-rasterbar.pc)

string (COMPARE EQUAL "${CMAKE_SIZEOF_VOID_P}" "8" IS64BITS)

if (IS64BITS AND RESPECTLIB64)
    set(LIBDIR "lib64")
else()
    set(LIBDIR "lib")
endif()

install(TARGETS libtorrent DESTINATION ${LIBDIR})
install(DIRECTORY include/libtorrent
    DESTINATION include
    PATTERN ".svn" EXCLUDE)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libtorrent-rasterbar.pc DESTINATION ${LIBDIR}/pkgconfig)

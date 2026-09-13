package Argon::IO::PerlExterns;
use 5.020;
use strict;
use warnings;

sub argon_externs {
    return {
        functions => {},
        methods => {},
        static_methods => {
            path_from_string => sub {
                my ($path) = @_;
                my $descriptor = Argon::Runtime::descriptor('perl-path', 'external', [], {});
                return bless {
                    descriptor => $descriptor,
                    __dispatch => sub {
                        my ($method) = @_;
                        return $path if $method =~ /^display__/;
                        if ($method =~ /^open__(read|write)__/ ) {
                            my $mode = $1 eq 'read' ? '<:raw' : '>:raw';
                            open my $file, $mode, $path or die Argon::Runtime::foreign_exception("$!");
                            my $stream_descriptor = Argon::Runtime::descriptor("perl-$1-stream", 'external', [], {});
                            my $stream = bless {
                                descriptor => $stream_descriptor,
                                __dispatch => sub {
                                    my ($stream_method, $array, $offset, $count) = @_;
                                    if ($stream_method =~ /^read__/) {
                                        my $buffer = '';
                                        my $read = read($file, $buffer, 0 + $count);
                                        die Argon::Runtime::foreign_exception("$!") unless defined $read;
                                        my @bytes = unpack('C*', $buffer);
                                        @{$array}[0 + $offset .. 0 + $offset + $read - 1] = @bytes if $read;
                                        return Math::BigInt->new($read);
                                    }
                                    if ($stream_method =~ /^write__/) {
                                        my $buffer = pack('C*', @{$array}[0 + $offset .. 0 + $offset + $count - 1]);
                                        my $written = syswrite($file, $buffer);
                                        die Argon::Runtime::foreign_exception("$!") unless defined $written && $written == length($buffer);
                                        return [];
                                    }
                                    die Argon::Runtime::foreign_exception("unknown stream method $stream_method");
                                },
                            }, 'Argon::Runtime::ExternalValue';
                            my $resource_descriptor = Argon::Runtime::descriptor("perl-$1-resource", 'external', [], {});
                            return bless {
                                descriptor => $resource_descriptor,
                                __dispatch => sub {
                                    my ($resource_method) = @_;
                                    return $stream if $resource_method =~ /^resource__/;
                                    if ($resource_method =~ /^close__/) { close $file; return []; }
                                    die Argon::Runtime::foreign_exception("unknown resource method $resource_method");
                                },
                            }, 'Argon::Runtime::ExternalValue';
                        }
                        die Argon::Runtime::foreign_exception("unknown path method $method");
                    },
                }, 'Argon::Runtime::ExternalValue';
            },
        },
    };
}

1;

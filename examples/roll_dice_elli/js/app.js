import 'htmx.org';
import { ConsoleSpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base';
import { WebTracerProvider } from '@opentelemetry/sdk-trace-web';
import { XMLHttpRequestInstrumentation } from '@opentelemetry/instrumentation-xml-http-request';
import { DocumentLoadInstrumentation } from '@opentelemetry/instrumentation-document-load';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { registerInstrumentations } from '@opentelemetry/instrumentation';
import { ZoneContextManager } from '@opentelemetry/context-zone';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
import * as api from '@opentelemetry/api';

const providerWithZone = new WebTracerProvider({
  resource: new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: 'roll-dice-ui',
  }),
});

providerWithZone.addSpanProcessor(new SimpleSpanProcessor(new ConsoleSpanExporter()));
providerWithZone.addSpanProcessor(new SimpleSpanProcessor(new OTLPTraceExporter({
  url: "http://localhost:4318/v1/traces"
})));

providerWithZone.register({
  contextManager: new ZoneContextManager()
});

registerInstrumentations({
  instrumentations: [
    new DocumentLoadInstrumentation(),
    new XMLHttpRequestInstrumentation({
      ignoreUrls: [/localhost/],
      propagateTraceHeaderCorsUrls: [
        'http://localhost:3000',
      ],
    }),
  ],
  tracerProvider: providerWithZone,
});


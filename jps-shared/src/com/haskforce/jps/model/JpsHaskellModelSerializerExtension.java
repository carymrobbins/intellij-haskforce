package com.haskforce.jps.model;

import com.haskforce.eta.jps.model.JpsEtaBuildOptionsSerializer;
import com.haskforce.eta.jps.model.JpsEtlasModuleType;
import com.intellij.util.xmlb.SkipDefaultValuesSerializationFilters;
import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsElement;
import org.jetbrains.jps.model.JpsElementFactory;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.serialization.JpsModelSerializerExtension;
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer;
import org.jetbrains.jps.model.serialization.facet.JpsFacetConfigurationSerializer;
import org.jetbrains.jps.model.serialization.library.JpsSdkPropertiesSerializer;
import org.jetbrains.jps.model.serialization.module.JpsModulePropertiesSerializer;
import org.jetbrains.jps.model.serialization.module.JpsModuleSourceRootDummyPropertiesSerializer;
import org.jetbrains.jps.model.serialization.module.JpsModuleSourceRootPropertiesSerializer;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Main entry point for the serializer server.
 */
public class JpsHaskellModelSerializerExtension extends JpsModelSerializerExtension {
    public static final String HASKELL_SDK_TYPE_ID = "Haskell SDK";

    @NotNull
    @Override
    public List<? extends JpsModuleSourceRootPropertiesSerializer<?>> getModuleSourceRootPropertiesSerializers() {
        return Collections.singletonList(new JpsModuleSourceRootDummyPropertiesSerializer(HaskellIncludeSourceRootType.INSTANCE, "haskell-include"));
    }

    @NotNull
    @Override
    public List<? extends JpsModulePropertiesSerializer<?>> getModulePropertiesSerializers() {
        return Arrays.asList(
            new JpsDummyModulePropertiesSerializer(JpsHaskellModuleType.INSTANCE, "HASKELL_MODULE"),
            new JpsDummyModulePropertiesSerializer(JpsEtlasModuleType.INSTANCE(), "ETLAS_MODULE")
        );
    }

    @NotNull
    @Override
    public List<? extends JpsSdkPropertiesSerializer<?>> getSdkPropertiesSerializers() {
        return Collections.singletonList(new JpsSdkPropertiesSerializer<JpsDummyElement>(HASKELL_SDK_TYPE_ID, JpsHaskellSdkType.INSTANCE) {
            @NotNull
            @Override
            public JpsDummyElement loadProperties(@Nullable Element propertiesElement) {
                return JpsElementFactory.getInstance().createDummyElement();
            }

            @Override
            public void saveProperties(@NotNull JpsDummyElement properties, @NotNull Element element) {
            }
        });
    }

    @NotNull
    @Override
    public List<? extends JpsFacetConfigurationSerializer<?>> getFacetConfigurationSerializers() {
        return Collections.singletonList(new JpsFacetConfigurationSerializer<JpsHaskellModuleExtension>(JpsHaskellModuleExtension.ROLE, HaskellFacetConstants.ID, HaskellFacetConstants.NAME) {
            @Override
            protected JpsHaskellModuleExtension loadExtension(@NotNull Element facetConfigurationElement, String name, JpsElement parent, JpsModule module) {
                HaskellModuleExtensionProperties props = XmlSerializer.deserialize(facetConfigurationElement, HaskellModuleExtensionProperties.class);
                return new JpsHaskellModuleExtension(props == null ? new HaskellModuleExtensionProperties() : props);
            }

            @Override
            protected void saveExtension(JpsHaskellModuleExtension extension, Element facetConfigurationTag, JpsModule module) {
                XmlSerializer.serializeInto(extension.getProperties(), facetConfigurationTag, new SkipDefaultValuesSerializationFilters());
            }
        });
    }

    @NotNull
    @Override
    public List<? extends JpsProjectExtensionSerializer> getProjectExtensionSerializers() {
        return Arrays.asList(
            new JpsHaskellBuildOptionsSerializer(),
            new JpsEtaBuildOptionsSerializer()
        );
    }
}

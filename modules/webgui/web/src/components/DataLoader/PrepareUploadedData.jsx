import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import DataManager from '../../api/DataManager';
import { UploadDataItemScript, ValuePlaceholder } from '../../api/keys';
import { removeLineBreakCharacters, getDirectoryLeaf, getFileBasename, backSlashesToForward, handleReceivedJSON, stringArrayToArray } from './utils/helpers';

import Row from '../common/Row/Row';
import Input from '../common/Input/Input/Input';
import styles from './PrepareUploadedData.scss';
import Button from '../common/Input/Button/Button';
import RadioButtons from '../common/Input/RadioButtons/RadioButtons';
import Label from '../common/Label/Label';
import Window from '../common/Window/Window';
import Column from './components/FlexColumn';
import ImageSelect from './components/ImageSelect';
import ProgressBar from '../common/ProgressBar/ProgressBar';
import Checkbox from '../common/Input/Checkbox/Checkbox';
import provideWindowWidth from './HOC/provideWindowSize';
import MultiInputs from './presentational/MultiInputs';
import Variables from './presentational/Variables';
import Translation from './presentational/Translation';
// import NumericInput from '../common/Input/NumericInput/NumericInput';

import {
  KEY_DIMENSIONS,
  KEY_UPPER_DOMAIN_BOUNDS,
  KEY_LOWER_DOMAIN_BOUNDS,
  KEY_SPICE_TRANSLATION,
  KEY_STATIC_TRANSLATION
} from './constants';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);

    this.state = {
      activated: false,
      volumeProgress: 0,
      metaData: {
        gridType: ''
      },
      uploadButtonIsClicked: false,
      itemName: '',
      tfLinkList: [{
        image: '',
        path: ''
      }],
      activeTfFilePath: '',
      translationType: KEY_STATIC_TRANSLATION,
      translationPos: { x: 0, y: 0, z: 0 },
      translationTarget: 'SUN',
      translationObserver: 'SUN',
      scale: 1,

      data: {
        dimensions: { x: 100, y: 100, z: 128 },
        lowerDomainBounds: { r: 1, theta: -90, phi: 0 },
        upperDomainBounds: { r: 15, theta: 90, phi: 360 },
        variable: 'rho',
        rSquared: false,
      }
    };


    this.onChangeMultiInputs = this.onChangeMultiInputs.bind(this);
    this.changeVariable = this.changeVariable.bind(this);
    this.changeRSquared = this.changeRSquared.bind(this);
    this.changeItemName = this.changeItemName.bind(this);
    this.handleGridTypeChange = this.handleGridTypeChange.bind(this);
    this.getDefaultItemName = this.getDefaultItemName.bind(this);
    this.upload = this.upload.bind(this);
    this.handleProgressValue = this.handleProgressValue.bind(this);
    this.subscribeToVolumeConversionProgress = this.subscribeToVolumeConversionProgress.bind(this);
    this.handleTranslationTypeChange = this.handleTranslationTypeChange.bind(this);
    this.handleSetStaticTranslation = this.handleSetStaticTranslation.bind(this);
    this.handleSetTranslationTarget = this.handleSetTranslationTarget.bind(this);
    this.handleSelectedTFImage = this.handleSelectedTFImage.bind(this);
    this.handleTfPresetsJSON = this.handleTfPresetsJSON.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { selectedFilePaths } = this.props;

    // const hasFilePaths = selectedFilePaths.length > 0 && selectedFilePaths[0] !== '';
    const hasFilePaths = selectedFilePaths.length > 0;
    const filePathsDiffer = JSON.stringify(selectedFilePaths) !== JSON.stringify(prevProps.selectedFilePaths);

    if (hasFilePaths && filePathsDiffer) {
      this.setState({
        activated: true,
        uploadButtonIsClicked: false
      });
    }

    if (prevProps.metaDataStringifiedJSON !== this.props.metaDataStringifiedJSON) {
      const metaData = this.props.metaDataStringifiedJSON ? handleReceivedJSON(this.props.metaDataStringifiedJSON) : undefined
      console.log(metaData)

      let newDimensions
      if (metaData && metaData.gridSystem1DimensionSize && metaData.gridSystem2DimensionSize && metaData.gridSystem3DimensionSize) {
        newDimensions = {
          x: metaData.gridSystem1DimensionSize,
          y: metaData.gridSystem2DimensionSize,
          z: metaData.gridSystem3DimensionSize,
        }
      }

      this.setState({
        metaData,
        data: { ...this.state.data, dimensions: newDimensions }
      })
    }

    this.subscribeToVolumeConversionProgress();
  }

  componentDidMount() {
    DataManager.trigger('Modules.DataLoader.ReadTransferFunctionPresets');
    DataManager.subscribe('Modules.DataLoader.TransferFunctionPresetsJSON', this.handleTfPresetsJSON);
  }

  handleTfPresetsJSON(jsonData) {
    const parsedJson = handleReceivedJSON(jsonData.Value);
    this.setState({ tfLinkList: parsedJson });
  }

  subscribeToVolumeConversionProgress() {
    DataManager.subscribe('Modules.DataLoader.Loader.VolumeConversionProgress', this.handleProgressValue);
  }

  handleProgressValue(data) {
    this.setState({ volumeProgress: data.Value });
  }

  onChangeMultiInputs({ currentTarget }, dataToChange) {
    const keyToChange = currentTarget.attributes.label.nodeValue;
    const valueToSet = Number(currentTarget.value);

    this.setState({
      data: {
        ...this.state.data,
        [dataToChange]: {
          ...this.state.data[dataToChange],
          [keyToChange]: valueToSet
        }
      }
    });
  }

  changeVariable(event) {
    this.setState({ data: { ...this.state.data, variable: event.value } });
  }

  changeItemName(event) {
    this.setState({ itemName: event.target.value });
  }

  changeRSquared(checked) {
    this.setState({ data: { ...this.state.data, rSquared: checked } });
  }

  handleGridTypeChange(option) {
    this.setState({ gridType: option });
  }

  getDefaultItemName() {

    // First render base case
    if (this.props.selectedFilePaths[0] === '') {
      const today = new Date();
      return `Unnamed_volume_${today.getFullYear()}_${today.getMonth()}_${today.getDay()}__${today.getHours()}_${today.getMinutes()}`;
    }

    return `${getFileBasename(getDirectoryLeaf(this.props.selectedFilePaths[0]))}_${this.state.data.variable}`
  }

  handleTranslationTypeChange(target) {
    let translationType;
    target === 'Spice'
      ? translationType = KEY_SPICE_TRANSLATION
      : translationType = KEY_STATIC_TRANSLATION;
    this.setState({ translationType });
  }

  handleSetStaticTranslation({ currentTarget }) {
    const { translationPos } = this.state;
    let label = currentTarget.attributes.label.value;
    let value = Number(currentTarget.value);
    this.setState({ translationPos: { ...translationPos, [label]: value } })
  }

  handleSetTranslationTarget(event) {
    this.setState({ translationTarget: event.value })
  }

  handleSelectedTFImage(imgSource) {
    let activeTfFilePath = '';
    this.state.tfLinkList.map(pair => {
      if (pair.image === imgSource) {
        activeTfFilePath = pair.path;
      }
    })

    activeTfFilePath = backSlashesToForward(activeTfFilePath)

    this.setState({ activeTfFilePath });
  }

  upload() {
    this.setState({ uploadButtonIsClicked: true });

    const { translationType, translationPos, translationTarget, translationObserver, activeTfFilePath } = this.state;
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, rSquared } = this.state.data;

    let payload = `\'
      return {
        ItemName="${this.state.itemName || this.getDefaultItemName()}",
        GridType="${this.state.gridType}",
        Translation={
          Type="${translationType}",
          ${translationType === KEY_STATIC_TRANSLATION ? `
          Position={${translationPos.x}, ${translationPos.y}, ${translationPos.z}},
          ` : `
          Target="${translationTarget}",
          Observer="${translationObserver}",
          `}
        },
        Task={
          Dimensions={${dimensions.x}, ${dimensions.y}, ${dimensions.z}}, 
          Variable="${variable.toLowerCase()}",
          LowerDomainBound={${lowerDomainBounds.r}, ${lowerDomainBounds.theta}, ${lowerDomainBounds.phi}}, 
          UpperDomainBound={${upperDomainBounds.r}, ${upperDomainBounds.theta}, ${upperDomainBounds.phi}}, 
          FactorRSquared="${rSquared.toString()}",          
        },
        Scale=${this.state.scale},
        TransferFunctionPath="${activeTfFilePath}"
      }
    \'`

    payload = removeLineBreakCharacters(payload);

    const payloadScript = UploadDataItemScript.replace(ValuePlaceholder, payload);
    DataManager.runScript(payloadScript);
  }

  render() {
    const { width, currentVolumesConvertedCount, currentVolumesToConvertCount } = this.props;
    const { volumeProgress, translationType, translationPos, translationTarget, metaData } = this.state;
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds } = this.state.data;
    // const spiceOptions = 'SUN EARTH'.split(' ').map(v => ({ value: v, label: v }));

    console.log(metaData)

    const isUnEditable = (this.state.uploadButtonIsClicked && (currentVolumesConvertedCount !== currentVolumesToConvertCount));

    const WINDOW_MAX_WIDTH = 800;
    const w = width / 2;
    const h = 700;
    const windowSize = {
      width: w > WINDOW_MAX_WIDTH ? WINDOW_MAX_WIDTH : w,
      height: h
    }

    const volumeProgressPercent = Math.floor(volumeProgress * 100);

    if (!this.state.activated) {
      return null;
    }

    const IMAGE_URI_BASE = 'http://localhost:1337';
    const tfImages = [
      {
        label: 'MAS',
        images: [
          `${IMAGE_URI_BASE}/mas_mhd_r_squared.png`,
          `${IMAGE_URI_BASE}/mas_mhd_temperature.png`,
          `${IMAGE_URI_BASE}/mas_mhd_velocity.png`
        ],
      },
      {
        label: 'ENLIL',
        images: [
          `${IMAGE_URI_BASE}/enlil.png`
        ]
      }
    ]

    return (
      <Window type="small"
        title="Prepare Data"
        size={windowSize}
        position={{ x: 100, y: 200 }}
        closeCallback={() => this.setState({ activated: false })}>
        <Column widthPercentage={100} className={styles.content}>
          <Row>
            <Column widthPercentage={50} className={styles.spaceFirstChildren}>
              <div>
                <Input onChange={(event) => this.changeItemName(event)}
                  label='Item name'
                  placeholder='name'
                  value={this.state.itemName || this.getDefaultItemName()} />
              </div>
              <div>
                <Row><Label>Visualization grid type </Label></Row>
                {(metaData && metaData.gridType) ? metaData.gridType : 'undefined'}
              </div>
              <MultiInputs presentationLabel='Data Dimensions'
                inputLabels={(metaData && metaData.gridSystem) ? metaData.gridSystem : ['x', 'y', 'z']}
                options={dimensions}
                disabled={isUnEditable}
                onChange={(target) => this.onChangeMultiInputs(target, KEY_DIMENSIONS)} />
              <Variables variable={variable}
                options={metaData ? metaData.variableKeys : undefined}
                disabled={isUnEditable}
                onChange={this.changeVariable} />
              <MultiInputs presentationLabel='Lower Domain Bounds'
                options={lowerDomainBounds}
                disabled={isUnEditable}
                onChange={(target) => this.onChangeMultiInputs(target, KEY_LOWER_DOMAIN_BOUNDS)} />
              <MultiInputs presentationLabel='Upper Domain Bounds'
                options={upperDomainBounds}
                disabled={isUnEditable}
                onChange={(target) => this.onChangeMultiInputs(target, KEY_UPPER_DOMAIN_BOUNDS)} />
              <Translation translationType={translationType}
                translationPos={translationPos}
                onTranslationTypeChange={this.handleTranslationTypeChange}
                onSetTranslation={(target) => this.handleSetStaticTranslation(target)}
                onSetTranslationTarget={this.handleSetTranslationTarget}
                target={translationTarget} />
              <Checkbox label={"Multiply " + variable + " with radius^2?"}
                onChange={this.changeRSquared}
                disabled={isUnEditable} />
            </Column>
            <Column className={styles.spaceFirstChildren}
              widthPercentage={50}>
              <Label>Here be trafnser functions</Label>
              <ImageSelect imageSources={tfImages}
                onSelect={this.handleSelectedTFImage} />
            </Column>
          </Row>
          <Column className={styles.footer}>
            <Button onClick={() => this.upload()}
              className={styles.convertButton}
              disabled={isUnEditable}>
              Convert
            </Button>
            {this.state.uploadButtonIsClicked && (
              <div>
                <Row>
                  <ProgressBar label='Volume conversion progress'
                    initializingMsg='Reading'
                    progressPercent={volumeProgressPercent} />
                </Row>
                <Row className={styles.filesConverted}>
                  <Label size="small">{currentVolumesConvertedCount} / {currentVolumesToConvertCount} files complete</Label>
                </Row>
              </div>
            )}
          </Column>
        </Column>
      </Window>
    );
  }
}

PrepareUploadedData.propTypes = {
  selectedFilePaths: PropTypes.arrayOf(PropTypes.string),
  currentVolumesConvertedCount: PropTypes.number,
  currentVolumesToConvertCount: PropTypes.number,
  width: PropTypes.number,
  height: PropTypes.number
};

PrepareUploadedData.defaultProps = {
  selectedFilePaths: [],
  currentVolumesConvertedCount: 0,
  currentVolumesToConvertCount: 0
}

const mapStateToProps = state => ({
  selectedFilePaths: state.dataLoader.selectedFilePaths,
  metaDataStringifiedJSON: state.dataLoader.selectedDataMetaData,
  currentVolumesConvertedCount: state.dataLoader.currentVolumesConvertedCount,
  currentVolumesToConvertCount: state.dataLoader.currentVolumesToConvertCount,
});

PrepareUploadedData = connect(
  mapStateToProps,
  null
)(PrepareUploadedData);

export default provideWindowWidth(PrepareUploadedData);

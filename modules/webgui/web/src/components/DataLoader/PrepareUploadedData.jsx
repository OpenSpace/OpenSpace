import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import DataManager from '../../api/DataManager';
import { UploadDataItemScript, ValuePlaceholder } from '../../api/keys';
import { removeLineBreakCharacters, getDirectoryLeaf, getFileBasename, backSlashesToForward, handleReceivedJSON } from './utils/helpers';

import Row from '../common/Row/Row';
import Input from '../common/Input/Input/Input';
import styles from './PrepareUploadedData.scss';
import Button from '../common/Input/Button/Button';
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
import LoadingString from '../common/LoadingString/LoadingString';
// import NumericInput from '../common/Input/NumericInput/NumericInput';

import {
  KEY_DIMENSIONS,
  KEY_UPPER_DOMAIN_BOUNDS,
  KEY_LOWER_DOMAIN_BOUNDS,
  KEY_SPICE_TRANSLATION,
} from './constants';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);

    this.state = {
      activated: false,
      volumeProgress: 0,
      metaData: {
        modelName: '',
        gridType: '',
        radiusUnit: '',
        gridSystem: ['x', 'y', 'z'],
        variableMinBounds: {},
        variableMaxBounds: {}
      },
      uploadButtonIsClicked: false,
      itemName: '',
      tfLinkList: [{
        image: '',
        path: ''
      }],
      activeTfFilePath: '',
      translationTarget: 'SUN',
      translationObserver: 'SUN',
      scale: 1,

      data: {
        dimensions: {},
        lowerDomainBounds: {},
        upperDomainBounds: {},
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
    this.handleSelectedTFImage = this.handleSelectedTFImage.bind(this);
    this.handleTfPresetsJSON = this.handleTfPresetsJSON.bind(this);
    this.handleSetTranslationTarget = this.handleSetTranslationTarget.bind(this);
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

    if (this.props.isReadingNewMetaData && (this.props.isReadingNewMetaData !== prevProps.isReadingNewMetaData)) {
      this.setState({
        data: {
          ...this.state.data,
          dimensions: { x: undefined, y: undefined, z: undefined },
          lowerDomainBounds: { x: undefined, y: undefined, z: undefined },
          upperDomainBounds: { x: undefined, y: undefined, z: undefined },
        }
      })
    }

    if (prevProps.metaDataStringifiedJSON !== this.props.metaDataStringifiedJSON && this.props.metaDataStringifiedJSON !== '') {
      const metaData = this.props.metaDataStringifiedJSON ? handleReceivedJSON(this.props.metaDataStringifiedJSON) : undefined
      const { dataDimensions, variableMinBounds, variableMaxBounds } = metaData;

      this.setState({
        metaData,
        data: {
          ...this.state.data,
          variable: 'rho',
          dimensions: { ...dataDimensions },
          lowerDomainBounds: { ...variableMinBounds },
          upperDomainBounds: { ...variableMaxBounds },
        }
      }, () => console.log(this.state))
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

    const { translationTarget, translationObserver, activeTfFilePath } = this.state;
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, rSquared } = this.state.data;

    let payload = `\'
      return {
        ItemName="${this.state.itemName || this.getDefaultItemName()}",
        GridType="${this.state.gridType}",
        Translation={
          Type="${KEY_SPICE_TRANSLATION}",
          Target="${translationTarget}",
          Observer="${translationObserver}",
          }
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
    const { width, currentVolumesConvertedCount, currentVolumesToConvertCount, isReadingNewMetaData } = this.props;
    const { volumeProgress, translationTarget, metaData } = this.state;
    const { variable, dimensions, lowerDomainBounds, upperDomainBounds } = this.state.data;
    // const spiceOptions = 'SUN EARTH'.split(' ').map(v => ({ value: v, label: v }));

    const isUnEditable = (this.state.uploadButtonIsClicked && (currentVolumesConvertedCount !== currentVolumesToConvertCount)) || isReadingNewMetaData;

    const WINDOW_MAX_WIDTH = 800;
    const w = width / 1.5;
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
            <Column widthPercentage={33} className={styles.spaceFirstChildren}>
              <div>
                <Label size='large'>Parameters</Label>
              </div>
              <div>
                <Input onChange={(event) => this.changeItemName(event)}
                  label='Item name'
                  placeholder='name'
                  disabled={isUnEditable}
                  value={this.state.itemName || this.getDefaultItemName()} />
              </div>
              <MultiInputs presentationLabel='Data Dimensions'
                description={"The number of cells in each dimension in the output volume"}
                inputTypes={metaData.gridSystem}
                options={dimensions}
                disabled={isUnEditable}
                loading={isReadingNewMetaData}
                onChange={(target) => this.onChangeMultiInputs(target, KEY_DIMENSIONS)} />
              <Variables variable={variable}
                options={metaData ? metaData.variableKeys : undefined}
                disabled={isUnEditable}
                onChange={this.changeVariable} />
              {variable == "rho" &&
                <div>
                  <Checkbox label={"Multiply with radius^2?"}
                    onChange={this.changeRSquared}
                    disabled={isUnEditable} />
                </div>
              }
              <MultiInputs presentationLabel='Lower Domain Bounds'
                description={"Lower visualization boundary limit" + (metaData ? " in " + metaData.radiusUnit : "")}
                inputTypes={metaData.gridSystem}
                options={lowerDomainBounds}
                disabled={isUnEditable}
                loading={isReadingNewMetaData}
                onChange={(target) => this.onChangeMultiInputs(target, KEY_LOWER_DOMAIN_BOUNDS)} />
              <MultiInputs presentationLabel='Upper Domain Bounds'
                description={"Upper visualization boundary limit" + (metaData ? " in " + metaData.radiusUnit : "")}
                inputTypes={metaData.gridSystem}
                options={upperDomainBounds}
                disabled={isUnEditable}
                loading={isReadingNewMetaData}
                onChange={(target) => this.onChangeMultiInputs(target, KEY_UPPER_DOMAIN_BOUNDS)} />
              <Translation
                onSetTranslationTarget={this.handleSetTranslationTarget}
                target={translationTarget} />
            </Column>
            <Column className={styles.spaceFirstChildren}
              widthPercentage={33}>
              <div>
                <Label size='large'>Information</Label>
              </div>
              <div>
                <Row><Label>Model:</Label></Row>
                <LoadingString loading={isReadingNewMetaData}>
                  {metaData.modelName.toUpperCase()}
                </LoadingString>
              </div>
              <div>
                <Row><Label>Visualization grid type: </Label></Row>
                <LoadingString loading={isReadingNewMetaData}>
                  {(metaData && metaData.gridType) ? metaData.gridType : 'undefined'}
                </LoadingString>
              </div>
              <div>
                <Row><Label>Unit:</Label></Row>
                <LoadingString loading={isReadingNewMetaData}>
                  {metaData.radiusUnit}
                </LoadingString>
              </div>
            </Column>
            <Column className={styles.spaceFirstChildren}
              widthPercentage={33}>
              <div>
                <Label size='large'>Transfer Function</Label>
              </div>
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
  isReadingNewMetaData: state.dataLoader.readingNewMetaData,
  metaDataStringifiedJSON: state.dataLoader.selectedDataMetaData,
  currentVolumesConvertedCount: state.dataLoader.currentVolumesConvertedCount,
  currentVolumesToConvertCount: state.dataLoader.currentVolumesToConvertCount,
});

PrepareUploadedData = connect(
  mapStateToProps,
  null
)(PrepareUploadedData);

export default provideWindowWidth(PrepareUploadedData);
